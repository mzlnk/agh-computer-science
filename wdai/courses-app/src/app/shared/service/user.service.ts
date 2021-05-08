import {Injectable} from '@angular/core';
import {Observable, of} from 'rxjs';
import {map, switchMap} from 'rxjs/operators';
import {AngularFireAuth} from '@angular/fire/auth';
import {Router} from '@angular/router';
import {AngularFirestore} from '@angular/fire/firestore';
import {User} from '../model/user.model';


@Injectable({
  providedIn: 'root'
})
export class UserService {

  private user: Observable<User>;

  constructor(private angularFireAuth: AngularFireAuth,
              private firestore: AngularFirestore,
              private router: Router) {

    this.user = this.angularFireAuth.authState.pipe(switchMap(user => {
      if (user) {
        return this.firestore.doc<User>(`users/${user.uid}`).valueChanges();
      } else {
        return of(null);
      }
    }));
  }

  public currentUser() {
    return this.user;
  }

  public createOrUpdateUser(user: User): void {
    this.firestore.doc(`users/${user.uid}`).set(user);
  }

  public signIn(email: string, password: string): any {
    return this.angularFireAuth.auth.signInWithEmailAndPassword(email, password)
      .then((result) => {
        localStorage.setItem('user', JSON.stringify(result));
        this.router.navigate(['']);
      })
      .catch((error) => {
        console.log(error.message);
      });
  }

  public signUp(email: string, password: string): any {
    return this.angularFireAuth.auth.createUserWithEmailAndPassword(email, password)
      .then((result) => {

        this.firestore.doc(`users/${result.user.uid}`).set({
          uid: result.user.uid,
          email: result.user.email,
          roles: ['user'],
          joinedCourses: [],
          ratedCourses: []
        });

        this.router.navigate(['']);
      })
      .catch((error) => window.alert(error.message));
  }

  public signOut(): any {
    return this.angularFireAuth.auth.signOut()
      .then(() => {
        localStorage.removeItem('user');
        this.router.navigate(['']);
      });
  }

  public canEditCourse(user: User): boolean {
    return user != null && user.roles.includes('admin');
  }

  public canSearchCourse(user: User): boolean {
    return user != null && user.roles.includes('user');
  }


}
