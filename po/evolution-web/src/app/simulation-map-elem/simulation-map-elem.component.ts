import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-simulation-map-elem',
  templateUrl: './simulation-map-elem.component.html',
  styleUrls: ['./simulation-map-elem.component.css']
})
export class SimulationMapElemComponent implements OnInit {

  uri: string;
  size: number;
  x: number;
  y: number;
  opacity: number;
  color: string;

  constructor() { }

  ngOnInit() {
  }

  getImgStyles() {
    return {
      position: 'absolute',
      height: String(this.size) + '%',
      width: String(this.size) + '%',
      fill: this.color,
      opacity: this.opacity,
      top: this.x + '%',
      left: this.y + '%',
      zIndex: 1
    };
  }

}
