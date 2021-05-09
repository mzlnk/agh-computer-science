import { Directive, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[entity-host]',
})
export class EntityDirective {
  constructor(public viewContainerRef: ViewContainerRef) { }
}
