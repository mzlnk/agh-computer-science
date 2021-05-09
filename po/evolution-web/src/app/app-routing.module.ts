import { NgModule } from '@angular/core';
import { HomeComponent} from './home/home.component';
import { SimulationComponent} from './simulation/simulation.component';
import {RouterModule, Routes} from '@angular/router';
import {SimulationDetailsGeneralComponent} from './simulation-details-general/simulation-details-general.component';
import {SimulationDetailsEntitiesComponent} from './simulation-details-entities/simulation-details-entities.component';
import {SimulationDetailsChartsComponent} from './simulation-details-charts/simulation-details-charts.component';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    component: HomeComponent
  },
  {
    path: 'simulation/:id',
    component: SimulationComponent,
    children: [
      {
        path: 'general',
        component: SimulationDetailsGeneralComponent,
      },
      {
        path: 'entities',
        component: SimulationDetailsEntitiesComponent,
      },
      {
        path: 'charts',
        component: SimulationDetailsChartsComponent,
      }
    ]
  }
];

@NgModule({
  declarations: [],
  imports: [
    RouterModule.forRoot(routes)
  ],
  exports: [
    RouterModule
  ]
})

export class AppRoutingModule { }
