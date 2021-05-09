import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { HomeComponent } from './home/home.component';
import { HomeSimulationListComponent } from './home-simulation-list/home-simulation-list.component';
import { HomeSimulationElemComponent } from './home-simulation-elem/home-simulation-elem.component';
import { SimulationComponent } from './simulation/simulation.component';
import { SimulationMapComponent } from './simulation-map/simulation-map.component';
import { SimulationDetailsGeneralComponent } from './simulation-details-general/simulation-details-general.component';
import { SimulationDetailsEntitiesComponent } from './simulation-details-entities/simulation-details-entities.component';
import { SimulationDetailsChartsComponent } from './simulation-details-charts/simulation-details-charts.component';
import { AppRoutingModule } from './app-routing.module';
import { SimulationDetailsEntitiesEntityElemComponent } from './simulation-details-entities-entity-elem/simulation-details-entities-entity-elem.component';
import {SimulationService} from './simulation.service';
import {EntityDirective} from './entity.directive';
import { SimulationMapElemComponent } from './simulation-map-elem/simulation-map-elem.component';
import {MapElemDirective} from './map-elem.directive';
import {SimulationDirective} from './simulation.directive';
import {SocketService} from './socket.service';

@NgModule({
  declarations: [
    AppComponent,
    HomeComponent,
    HomeSimulationListComponent,
    HomeSimulationElemComponent,
    SimulationComponent,
    SimulationMapComponent,
    SimulationDetailsGeneralComponent,
    SimulationDetailsEntitiesComponent,
    SimulationDetailsChartsComponent,
    SimulationDetailsEntitiesEntityElemComponent,
    EntityDirective,
    MapElemDirective,
    SimulationDirective,
    SimulationMapElemComponent
  ],
  entryComponents: [
    SimulationDetailsEntitiesEntityElemComponent,
    SimulationMapElemComponent,
    HomeSimulationElemComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
  ],
  providers: [
    SimulationService,
    SocketService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
