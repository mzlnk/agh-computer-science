import {Component, OnDestroy, OnInit} from '@angular/core';
import {ActivatedRoute} from '@angular/router';
import {CourseService} from '../shared/service/course.service';
import {StarRatingComponent} from 'ng-starrating';
import {UserService} from '../../shared/service/user.service';
import {User} from '../../shared/model/user.model';
import {Course} from '../shared/model/course.model';
import {switchMap} from 'rxjs/operators';
import {of} from 'rxjs';

@Component({
  selector: 'app-course-show',
  templateUrl: './course-show.component.html',
  styleUrls: ['./course-show.component.css']
})
export class CourseShowComponent implements OnInit {

  course: Course;
  user: User;

  rating: number;

  constructor(private route: ActivatedRoute,
              private courseService: CourseService,
              private userService: UserService) {
  }

  ngOnInit() {
    this.route.params.pipe(switchMap(params => {
      if (params && params.id) {
        return this.courseService.getCourse(params.id);
      } else {
        return of(null);
      }
    }))
      .subscribe(course => {
        if (course == null) {
          return;
        }

        this.course = course;
        this.rating = (this.course.rates !== 0 ? Math.round(this.course.rateSum / this.course.rates * 100) / 100 : 0);
      });

    this.userService.currentUser().subscribe(user => {
      this.user = user;
    });
  }

  onRate($event: { oldValue: number, newValue: number, starRating: StarRatingComponent }) {
    this.course.rateSum += $event.newValue;
    this.course.rates += 1;
    this.courseService.createOrUpdateCourse(this.course);

    this.user.ratedCourses.push(this.course.id);
    this.userService.createOrUpdateUser(this.user);
  };

  public joinCourse(): void {
    this.course.attendees += 1;
    this.courseService.createOrUpdateCourse(this.course);

    this.user.joinedCourses.push(this.course.id);
    this.userService.createOrUpdateUser(this.user);
  }

  public isCourseJoined(): boolean {
    return this.user != null && this.course != null && this.user.joinedCourses.includes(this.course.id);
  }

  public isCourseRated(): boolean {
    return this.user != null && this.course != null && this.user.ratedCourses.includes(this.course.id);
  }

  public isCourseAchievedAttendeeLimit(): boolean {
    return this.course != null && this.course.attendees >= this.course.attendeesLimit;
  }

}
