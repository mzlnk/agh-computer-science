import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {CourseListComponent} from './course-list/course-list.component';
import {CourseElemComponent} from './course-elem/course-elem.component';
import {CourseService} from './shared/service/course.service';
import {CourseSearchComponent} from './course-search/course-search.component';
import {CourseAddComponent} from './course-add/course-add.component';
import {CourseShowComponent} from './course-show/course-show.component';
import {RouterModule} from '@angular/router';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';
import {RatingModule} from 'ng-starrating';
import { CourseSearchFilterComponent } from './course-search-filter/course-search-filter.component';
import {CourseSearchFilterPipe} from './shared/pipe/course-search-filter.pipe';
import { CourseEditComponent } from './course-edit/course-edit.component';

@NgModule({
  declarations: [
    CourseListComponent,
    CourseElemComponent,
    CourseSearchComponent,
    CourseAddComponent,
    CourseShowComponent,
    CourseSearchFilterComponent,
    CourseSearchFilterPipe,
    CourseEditComponent
  ],
  imports: [
    CommonModule,
    RouterModule,
    RatingModule,
    FormsModule,
    ReactiveFormsModule,
  ],
  exports: [
    CourseListComponent,
    CourseElemComponent,
    CourseSearchComponent,
    CourseAddComponent,
    CourseShowComponent,
    CourseSearchFilterComponent
  ],
  providers: [
    CourseService,
    CourseSearchFilterPipe
  ],
})
export class CoursesModule {
}
