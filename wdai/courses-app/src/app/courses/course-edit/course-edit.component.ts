import {Component, OnInit} from '@angular/core';
import {FormBuilder} from '@angular/forms';
import {ActivatedRoute, Router} from '@angular/router';
import {CourseService} from '../shared/service/course.service';
import {switchMap} from 'rxjs/operators';
import {of} from 'rxjs';
import {Course} from '../shared/model/course.model';

@Component({
  selector: 'app-course-edit',
  templateUrl: './course-edit.component.html',
  styleUrls: ['./course-edit.component.css']
})
export class CourseEditComponent implements OnInit {

  editCourseForm;
  private course: Course;

  constructor(private formBuilder: FormBuilder,
              private router: Router,
              private courseService: CourseService,
              private route: ActivatedRoute) {
  }

  ngOnInit() {
    this.editCourseForm = this.formBuilder.group({
      courseName: '',
      courseDescription: '',
      photoUrl: '',
      courseEcts: '',
      courseSemester: '',
      courseAttendeesLimit: '',
      courseType: ''
    });

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

        this.editCourseForm = this.formBuilder.group({
          courseName: course.name,
          courseDescription: course.description,
          photoUrl: course.url,
          courseEcts: course.ects,
          courseSemester: course.semester,
          courseAttendeesLimit: course.attendeesLimit,
          courseType: course.courseType
        });
      });
  }

  onCourseEdit(courseData): void {
    if (this.editCourseForm.invalid) {
      alert('invalid form!');
      return;
    }

    this.courseService.createOrUpdateCourse({
      id: this.course.id,
      name: courseData.courseName,
      description: courseData.courseDescription,
      url: courseData.photoUrl,
      ects: courseData.courseEcts,
      semester: courseData.courseSemester,
      courseType: courseData.courseType,
      attendeesLimit: courseData.courseAttendeesLimit,
      attendees: 0,
      rateSum: this.course.rateSum,
      rates: this.course.rates
    });

    this.router.navigate(['/courses']);
    alert('Course updated!');
  }

}
