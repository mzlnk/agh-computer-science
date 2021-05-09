import { Directive, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[simulation-elem-host]',
})
export class SimulationDirective {
  constructor(public viewContainerRef: ViewContainerRef) { }
}
