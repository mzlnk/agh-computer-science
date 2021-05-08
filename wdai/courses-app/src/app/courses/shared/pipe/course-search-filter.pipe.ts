import {Pipe, PipeTransform} from '@angular/core';
import {Course} from '../model/course.model';

@Pipe({name: 'courseSearchFilter'})
export class CourseSearchFilterPipe implements PipeTransform {

  transform(courses: Course[], courseName: string, semesters: boolean[], ratings: boolean[], ects: boolean[]): Course[] {

    if (!courses) {
      return [];
    }

    let allNames = true;
    if (courseName) {
      allNames = false;
    }

    let allSemesters = true;
    for (let semester of semesters) {
      if (semester) {
        allSemesters = false;
        break;
      }
    }

    let allRatings = true;
    for(let rating of ratings) {
      if(rating) {
        allRatings = false;
        break;
      }
    }


    let allEcts = true;
    for(let e of ects) {
      if(e) {
        allEcts = false;
        break;
      }
    }

    courseName = courseName.toLowerCase();
    console.log('semesters: ' + semesters);

    return courses.filter(course => {
      if(!allNames && !course.name.toLowerCase().includes(courseName)) return false;
      if(!allSemesters && !semesters[course.semester]) return false;
      if(!allRatings && !ratings[Math.round(course.rates != 0 ? (course.rateSum / course.rates) : 0)]) return false;
      if(!allEcts && !ects[course.ects]) return false;

      return true;
    });
  }

}
