import {Component, EventEmitter, Input, OnInit, Output} from '@angular/core';
import {Course} from '../shared/model/course.model';
import {UserService} from '../../shared/service/user.service';
import {User} from '../../shared/model/user.model';
import {Router} from '@angular/router';

@Component({
  selector: 'app-course-elem',
  templateUrl: './course-elem.component.html',
  styleUrls: ['./course-elem.component.css']
})
export class CourseElemComponent implements OnInit {

  user: User;

  @Input() course: Course;
  @Output() courseRemoved = new EventEmitter<string>();

  constructor(public userService: UserService,
              private router: Router) {
  }

  ngOnInit() {
    this.userService.currentUser().subscribe(user => {
      this.user = user;
    })
  }

  removeCourse($event: MouseEvent): void {
    this.courseRemoved.emit(this.course.id);
    $event.stopPropagation();
  }

  editCourse($event: MouseEvent): void {
    this.router.navigate([`/courses/edit`, this.course.id]);
    $event.stopPropagation();
  }

}
