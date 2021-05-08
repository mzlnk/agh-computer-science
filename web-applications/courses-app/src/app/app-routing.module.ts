import {Component, NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterModule, Routes} from '@angular/router';
import {AppComponent} from './app.component';
import {CourseSearchComponent} from './courses/course-search/course-search.component';
import {CourseAddComponent} from './courses/course-add/course-add.component';
import {CourseShowComponent} from './courses/course-show/course-show.component';
import {WelcomeComponent} from './home/welcome/welcome.component';
import {SignInComponent} from './home/sign-in/sign-in.component';
import {SignUpComponent} from './home/sign-up/sign-up.component';
import {CourseEditComponent} from './courses/course-edit/course-edit.component';

import {AuthGuard} from './guard/auth.guard';
import {canActivate} from '@angular/fire/auth-guard';
import {NotAuthGuard} from './guard/not-auth.guard';
import {AdminAuthGuard} from './guard/admin-auth.guard';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    component: WelcomeComponent
  },
  {
    path: 'courses',
    component: CourseSearchComponent,
    canActivate: [AuthGuard]
  },
  {
    path: 'courses/add',
    component: CourseAddComponent,
    canActivate: [AdminAuthGuard]
  },
  {
    path: 'courses/edit/:id',
    component: CourseEditComponent,
    canActivate: [AdminAuthGuard]
  },
  {
    path: 'courses/show/:id',
    component: CourseShowComponent,
    canActivate: [AuthGuard]
  },
  {
    path: 'sign-in',
    component: SignInComponent,
    canActivate: [NotAuthGuard]
  },
  {
    path: 'sign-up',
    component: SignUpComponent,
    canActivate: [NotAuthGuard]
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

export class AppRoutingModule {
}
