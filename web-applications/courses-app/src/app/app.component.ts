import {Component, OnInit} from '@angular/core';
import * as AOS from 'aos';
import {UserService} from './shared/service/user.service';
import {User} from './shared/model/user.model';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {

  private user: User;

  public username: string;

  constructor(private userService: UserService) {
  }

  ngOnInit(): void {
    AOS.init();

    this.userService.currentUser().subscribe(user => {
      this.user = user;
      this.username = user ? user.email : '';

      console.log(`changed user status -> ${JSON.stringify(user)}`);
    });
  }

  public signOut(): any {
    return this.userService.signOut();
  }

  public canSearchCourses(): boolean {
    return this.userService.canSearchCourse(this.user);
  }

  public canEditCourses(): boolean {
    return this.userService.canEditCourse(this.user);
  }

  public isUserSignedIn(): boolean {
    return this.user != null;
  }

}
