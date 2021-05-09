import {Component, ComponentFactoryResolver, OnDestroy, OnInit, ViewChild} from '@angular/core';
import {SimulationDirective} from '../simulation.directive';
import {SimulationService} from '../simulation.service';
import {HomeSimulationElemComponent} from '../home-simulation-elem/home-simulation-elem.component';
import {Stomp} from '@stomp/stompjs';

@Component({
  selector: 'app-home-simulation-list',
  templateUrl: './home-simulation-list.component.html',
  styleUrls: ['./home-simulation-list.component.css']
})
export class HomeSimulationListComponent implements OnInit, OnDestroy {

  private serverUrl = 'ws://localhost:5000/evolution ';
  private stompClient;

  private loadComponents = (message) => {
    const componentFactory = this.componentFactoryResolver.resolveComponentFactory(HomeSimulationElemComponent);
    const viewContainerRef = this.simulationHost.viewContainerRef;
    viewContainerRef.clear();

    const data = JSON.parse(message.body);
    for (const sm of data) {
      console.log(sm);
      const componentRef = viewContainerRef.createComponent(componentFactory);
      (componentRef.instance as HomeSimulationElemComponent).id = sm.id;
      (componentRef.instance as HomeSimulationElemComponent).name = sm.name;
      (componentRef.instance as HomeSimulationElemComponent).currentAge = this.parseAge(sm.currentAge);
    }
  }

  @ViewChild(SimulationDirective, {static: true}) simulationHost: SimulationDirective;

  constructor(private componentFactoryResolver: ComponentFactoryResolver) {
  }

  ngOnInit() {
    this.initSocketConnection();
  }

  ngOnDestroy() {
    this.stompClient.disconnect();
  }

  private initSocketConnection() {
    this.stompClient = Stomp.client(this.serverUrl);
    this.stompClient.debug = (message) => {};
    this.stompClient.connect({}, (frame) => {
      this.stompClient.subscribe('/live/evolution/all/properties', this.loadComponents);
    });
  }

  private parseAge(currentAge: number): string {
    const years = Math.floor(currentAge / 360);
    const months = Math.floor((currentAge - years * 360) / 30);
    const days = currentAge - years * 360 - months * 30;

    return `${years} years ${months} months ${days} days`;
  }

}
