import { Component, OnInit } from '@angular/core';
import {ActivatedRoute} from '@angular/router';
import {SimulationService} from '../simulation.service';

@Component({
  selector: 'app-home-simulation-elem',
  templateUrl: './home-simulation-elem.component.html',
  styleUrls: ['./home-simulation-elem.component.css']
})
export class HomeSimulationElemComponent implements OnInit {

  id: string;
  name: string;
  currentAge: string;

  private sub: any;

  constructor(private simulationService: SimulationService) { }

  ngOnInit() { }

}
