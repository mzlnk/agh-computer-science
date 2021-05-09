import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'tr[app-simulation-details-entities-entity-elem]',
  templateUrl: './simulation-details-entities-entity-elem.component.html',
  styleUrls: ['./simulation-details-entities-entity-elem.component.css']
})
export class SimulationDetailsEntitiesEntityElemComponent implements OnInit {

  numId: number;
  age: number;
  generation: string;
  genes: number[] = [];
  energy: number;

  constructor() { }

  ngOnInit() { }

}
