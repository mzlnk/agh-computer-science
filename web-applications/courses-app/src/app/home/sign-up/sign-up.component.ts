import { Component, OnInit } from '@angular/core';
import {FormBuilder} from '@angular/forms';
import {UserService} from '../../shared/service/user.service';

@Component({
  selector: 'app-sign-up',
  templateUrl: './sign-up.component.html',
  styleUrls: ['./sign-up.component.css']
})
export class SignUpComponent implements OnInit {

  signUpForm: any;

  constructor(private formBuilder: FormBuilder,
              private userService: UserService) {

  }

  ngOnInit() {
    this.signUpForm = this.formBuilder.group({
      email: '',
      password: ''
    });
  }

  public onSignUp(signUpData): void {
    this.userService.signUp(signUpData.email, signUpData.password);
  }

}
