import {Component, Input, OnChanges, OnInit} from '@angular/core';
import {CourseService} from '../shared/service/course.service';
import {Course} from '../shared/model/course.model';
import {CourseSearchFilterPipe} from '../shared/pipe/course-search-filter.pipe';

@Component({
  selector: 'app-course-list',
  templateUrl: './course-list.component.html',
  styleUrls: ['./course-list.component.css']
})
export class CourseListComponent implements OnInit, OnChanges {

  private courses: Course[];

  @Input()
  filterData: any;

  filteredCourses: Course[];

  constructor(private courseService: CourseService, private courseSearchFilterPipe: CourseSearchFilterPipe) {
  }

  ngOnInit() {
    this.courseService.getCourses().subscribe(courses => {
      this.courses = courses;
      this.filterCourses();
    });
  }

  ngOnChanges() {
    this.filterCourses();
  }

  onCourseRemoved(id: string): void {
    this.courseService.removeCourse(id);
    // this.courses = this.courseService.getCourses(); // todo: check if correct way to update view
    this.filterCourses();
  }

  private filterCourses() {
    this.filteredCourses = this.courseSearchFilterPipe.transform(
      this.courses,
      this.filterData.chosenName,
      this.filterData.chosenSemesters,
      this.filterData.chosenRatings,
      this.filterData.chosenEcts
    );
  }

}
