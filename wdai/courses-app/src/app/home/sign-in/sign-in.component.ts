import { Component, OnInit } from '@angular/core';
import {FormBuilder} from '@angular/forms';
import {UserService} from '../../shared/service/user.service';

@Component({
  selector: 'app-sign-in',
  templateUrl: './sign-in.component.html',
  styleUrls: ['./sign-in.component.css']
})
export class SignInComponent implements OnInit {

  signInForm: any;

  constructor(private formBuilder: FormBuilder,
              private userService: UserService) {

  }

  ngOnInit() {
    this.signInForm = this.formBuilder.group({
      email: '',
      password: ''
    });
  }

  public onSignIn(signInData): void {
    this.userService.signIn(signInData.email, signInData.password);
  }


}
