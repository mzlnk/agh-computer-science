import {Component, OnDestroy, OnInit} from '@angular/core';

import * as AOS from 'aos';
import * as Stomp from 'stompjs';
import * as SockJS from 'sockjs-client';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit, OnDestroy {

  private serverUrl = 'ws://localhost:5000/evolution ';
  private stompClient;

  constructor() {
    this.initConnection();
  }

  ngOnInit() {
    AOS.init();
    // this.connect();
    this.initConnection();
  }

  ngOnDestroy() {
    this.stompClient.disconnect();
  }

  initConnection() {
    this.stompClient = Stomp.client(this.serverUrl);
    const that = this;
    this.stompClient.connect({}, (frame) => {
      console.log(frame);
      const callback = (message) => {
        console.log(message.body);
      };
      this.stompClient.debug = () => {};
      this.stompClient.subscribe('/live/simulation/all/properties', callback);
    });
  }

}
