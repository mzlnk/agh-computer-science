import {Component, OnDestroy, OnInit} from '@angular/core';
import {ActivatedRoute} from '@angular/router';
import {Stomp} from '@stomp/stompjs';

@Component({
  selector: 'app-simulation-details-general',
  templateUrl: './simulation-details-general.component.html',
  styleUrls: ['./simulation-details-general.component.css']
})
export class SimulationDetailsGeneralComponent implements OnInit, OnDestroy {

  id: string;
  name: string;
  timeCreated: string;
  lifetime: string;
  currentAge: string;
  mapSize: string;
  jungleSize: string;
  animalMaxEnergy: string;
  grassInitEnergy: string;
  animalInitEnergy: string;
  animalInitAmount: string;
  private serverUrl = 'ws://localhost:5000/evolution ';
  private stompClient;
  private sub: any;

  constructor(private route: ActivatedRoute) {
  }

  ngOnInit() {
    this.sub = this.route.parent.params.subscribe(params => {
      this.id = params.id;
      this.initSockectConnection();
    });
  }

  ngOnDestroy() {
    this.sub.unsubscribe();
    this.stompClient.disconnect();
  }

  initSockectConnection() {
    this.stompClient = Stomp.client(this.serverUrl);
    this.stompClient.debug = (message) => { };
    this.stompClient.connect({}, (frame) => {
      this.stompClient.subscribe('/live/evolution/id/' + this.id + '/properties', this.updateProperties);
    });
  }

  private updateProperties = (message) => {
    const properties = JSON.parse(message.body);

    this.name = properties.name;
    this.timeCreated = properties.timeCreated;
    this.lifetime = properties.lifetime;
    this.currentAge = properties.currentDay;
    this.mapSize = `${properties.mapSize}x${properties.mapSize}`;
    this.jungleSize = `${properties.jungleSize}x${properties.jungleSize}`;
    this.animalMaxEnergy = String(properties.maxAnimalEnergy);
    this.grassInitEnergy = String(properties.grassInitEnergy);
    this.animalInitEnergy = String(properties.animalInitEnergy);
    this.animalInitAmount = String(properties.animalInitAmount);
  }

}
