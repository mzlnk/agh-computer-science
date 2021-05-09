import {Component, ComponentFactoryResolver, NgZone, OnDestroy, OnInit, ViewChild} from '@angular/core';
import {MapElemDirective} from '../map-elem.directive';
import {ActivatedRoute} from '@angular/router';
import {SimulationMapElemComponent} from '../simulation-map-elem/simulation-map-elem.component';
import {Stomp} from '@stomp/stompjs';
import * as PIXI from 'pixi.js';

@Component({
  selector: 'app-simulation-map',
  templateUrl: './simulation-map.component.html',
  styleUrls: ['./simulation-map.component.css']
})
export class SimulationMapComponent implements OnInit, OnDestroy {

  private serverUrl = 'ws://localhost:5000/evolution ';
  private stompClient;

  private createGridSub;

  private app: PIXI.Application;

  private elems: PIXI.Sprite[] = [];

  private id: string;
  private mapSize: number;
  private animalColor;
  private grassColor;

  private sub: any;

  @ViewChild(MapElemDirective, {static: true}) mapElemHost: MapElemDirective;

  private createGrid = (message) => {
    const data = JSON.parse(message.body);
    console.log('map size: ' + data.mapSize);
    this.mapSize = data.mapSize;

    this.drawMap();

    this.stompClient.subscribe('/live/evolution/id/' + this.id + '/day', this.updateGrid);
    this.createGridSub.unsubscribe();
  };

  private updateGrid = (message) => {
    const entities = JSON.parse(message.body);
    for (const elem of this.elems) {
      elem.tint = 0xffffff;
    }

    for (let i = 0; i < entities.x.length; i++) {
      const idx = entities.x[i] * this.mapSize + entities.y[i];
      this.elems[idx].tint = (entities.type[i] === 'A' ? this.interpolate(0xffffff, 0xd21f3c, entities.energyFactor[i]) : 0x50c878);
    }
  };

  constructor(private componentFactoryResolver: ComponentFactoryResolver,
              private route: ActivatedRoute,
              private ngZone: NgZone) {
  }

  ngOnInit() {
    this.sub = this.route.params.subscribe(params => {
      this.id = params.id;
      this.ngZone.runOutsideAngular(() => {
        this.app = new PIXI.Application({
          width: window.innerWidth,
          height: window.innerWidth,
          transparent: true
        });
      });
      this.initSocketConnection();
    });
  }

  ngOnDestroy() {
    this.sub.unsubscribe();
    this.stompClient.disconnect();
  }

  drawMap() {
    const map = document.getElementById('map');

    this.app.view.style.width = '100%';
    this.app.view.style.height = '100%';
    map.appendChild(this.app.view);

    const unitSize = this.app.screen.width / this.mapSize;
    const margin = 0.05 * unitSize;

    for (let x = 0; x < this.mapSize; x++) {
      for (let y = 0; y < this.mapSize; y++) {
        const elem = new PIXI.Sprite(PIXI.Texture.WHITE);
        elem.x = x * unitSize + margin;
        elem.y = y * unitSize + margin;
        elem.width = unitSize - 2 * margin;
        elem.height = unitSize - 2 * margin;
        this.elems.push(elem);
        this.app.stage.addChild(elem);
      }
    }

  }

  initSocketConnection() {
    this.stompClient = Stomp.client(this.serverUrl);
    this.stompClient.debug = (message) => {};
    this.stompClient.connect({}, (frame) => {
      this.createGridSub = this.stompClient.subscribe('/live/evolution/id/' + this.id + '/properties', this.createGrid);
    });
  }

  interpolate(a: number, b: number, amount: number) {
    const ar = (a >> 16),
      ag = a >> 8 & 0xff,
      ab = a & 0xff,

      br = b >> 16,
      bg = b >> 8 & 0xff,
      bb = b & 0xff,

      rr = ar + amount * (br - ar),
      rg = ag + amount * (bg - ag),
      rb = ab + amount * (bb - ab);

    return (rr << 16) + (rg << 8) + (rb | 0);
  }

}
