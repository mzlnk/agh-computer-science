import { Directive, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[map-elem-host]',
})
export class MapElemDirective {
  constructor(public viewContainerRef: ViewContainerRef) { }
}
