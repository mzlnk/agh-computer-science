import {Component, OnInit} from '@angular/core';
import {FormBuilder} from '@angular/forms';
import {Router} from '@angular/router';
import {CourseService} from '../shared/service/course.service';

@Component({
  selector: 'app-course-add',
  templateUrl: './course-add.component.html',
  styleUrls: ['./course-add.component.css']
})
export class CourseAddComponent implements OnInit {

  addCourseForm;

  constructor(private formBuilder: FormBuilder,
              private router: Router,
              private courseService: CourseService) {
  }

  ngOnInit() {
    this.addCourseForm = this.formBuilder.group({
      courseName: '',
      courseDescription: '',
      photoUrl: '',
      courseEcts: '',
      courseSemester: '',
      courseAttendeesLimit: '',
      courseType: 'Lecture'
    });
  }

  onCourseAdd(courseData): void {
    if (this.addCourseForm.invalid) {
      return;
    }

    this.courseService.createOrUpdateCourse({
      id: this.courseService.randomId(),
      name: courseData.courseName,
      description: courseData.courseDescription,
      url: courseData.photoUrl,
      ects: courseData.courseEcts,
      semester: courseData.courseSemester,
      courseType: courseData.courseType,
      attendeesLimit: courseData.courseAttendeesLimit,
      attendees: 0,
      rates: 0,
      rateSum: 0
    });
    this.router.navigate(['/']);
    alert('Added new course!');
  }

}
