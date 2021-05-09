import {Component, ComponentFactoryResolver, Injectable, OnDestroy, OnInit, ViewChild} from '@angular/core';
import {SimulationDetailsEntitiesEntityElemComponent} from '../simulation-details-entities-entity-elem/simulation-details-entities-entity-elem.component';
import {EntityDirective} from '../entity.directive';
import {ActivatedRoute} from '@angular/router';
import {Stomp} from '@stomp/stompjs';

@Injectable({
  providedIn: 'root'
})
@Component({
  selector: 'app-simulation-details-entities',
  templateUrl: './simulation-details-entities.component.html',
  styleUrls: ['./simulation-details-entities.component.css']
})
export class SimulationDetailsEntitiesComponent implements OnInit, OnDestroy {

  id: string;
  @ViewChild(EntityDirective, {static: true}) entityHost: EntityDirective;
  private serverUrl = 'ws://localhost:5000/evolution ';
  private stompClient;
  private sub: any;

  more: boolean;
  otherCount: number;

  constructor(private componentFactoryResolver: ComponentFactoryResolver,
              private route: ActivatedRoute) {
  }

  ngOnInit() {
    this.sub = this.route.parent.params.subscribe(params => {
      this.id = params.id;
      this.initSocketConnection();
    });
  }

  ngOnDestroy() {
    this.sub.unsubscribe();
    this.stompClient.disconnect();
  }

  initSocketConnection() {
    this.stompClient = Stomp.client(this.serverUrl);
    this.stompClient.debug = (message) => {
    };
    this.stompClient.connect({}, (frame) => {
      this.stompClient.subscribe('/live/evolution/id/' + this.id + '/entities', this.updateEntities);
    });
  }

  private updateEntities = (message) => {
    const entities = JSON.parse(message.body);

    console.log(message.body);

    const componentFactory = this.componentFactoryResolver.resolveComponentFactory(SimulationDetailsEntitiesEntityElemComponent);
    const viewContainerRef = this.entityHost.viewContainerRef;
    viewContainerRef.clear();

    let i = 0;
    for (i = 0; i < entities.age.length; i++) {
      const componentRef = viewContainerRef.createComponent(componentFactory);
      (componentRef.instance as SimulationDetailsEntitiesEntityElemComponent).numId = (i + 1);
      (componentRef.instance as SimulationDetailsEntitiesEntityElemComponent).age = entities.age[i];
      (componentRef.instance as SimulationDetailsEntitiesEntityElemComponent).generation = entities.generation[i];
      (componentRef.instance as SimulationDetailsEntitiesEntityElemComponent).genes = entities.genes[i];
      (componentRef.instance as SimulationDetailsEntitiesEntityElemComponent).energy = entities.energy[i];
    }

    this.more = entities.more;
    this.otherCount = entities.count - i;
  };

}
