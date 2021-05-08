import {Injectable} from '@angular/core';
import {CoursesModule} from '../../courses.module';
// @ts-ignore
import * as coursesData from '../../../../assets/courses.json';
import {Course} from '../model/course.model';
import {AngularFirestore} from '@angular/fire/firestore';
import {Observable} from 'rxjs';
import {UUID} from 'angular2-uuid';


@Injectable({
  providedIn: 'root'
})
export class CourseService {

  private courses: any = {};

  constructor(private firestore: AngularFirestore) {
  }

  public getCourses(): Observable<Course[]> {
    return this.firestore.collection<Course>('courses').valueChanges();
  }

  public getCourse(id: string): Observable<Course> {
    return this.firestore.doc<Course>(`courses/${id}`).valueChanges();
    return this.courses[id];
  }

  public createOrUpdateCourse(course: Course): void {
    this.firestore.doc<Course>(`courses/${course.id}`).set(course);
  }

  public removeCourse(id: string): void {
    this.firestore.doc<Course>(`courses/${id}`).delete();
  }

  public randomId(): string {
    return UUID.UUID();
  }

}
