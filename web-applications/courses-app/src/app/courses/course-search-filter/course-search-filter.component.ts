import {Component, EventEmitter, OnInit, Output} from '@angular/core';

@Component({
  selector: 'app-course-search-filter',
  templateUrl: './course-search-filter.component.html',
  styleUrls: ['./course-search-filter.component.css']
})
export class CourseSearchFilterComponent implements OnInit {

  semesters: number[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
  ratings: number[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  ects: number[] = [0, 1, 2, 3, 4, 5, 6];

  @Output() filterDataChanged = new EventEmitter<any>();

  private chosenName: string = "";
  private chosenSemesters: boolean[] = [];
  private chosenRatings: boolean[] = [];
  private chosenEcts: boolean[] = [];

  constructor() { }

  ngOnInit() {
  }

  changeFilter(): void {
    this.filterDataChanged.emit({
      chosenName: this.chosenName,
      chosenSemesters: this.chosenSemesters,
      chosenRatings: this.chosenRatings,
      chosenEcts: this.chosenEcts
    })
  }

  changeName($event: any) {
    this.chosenName = $event;
    this.changeFilter();
  }

  checkSemester(which: number, isChecked: boolean) {
    this.chosenSemesters[which] = isChecked;
    this.changeFilter();
  }

  checkRating(which: number, isChecked: boolean) {
    this.chosenRatings[which] = isChecked;
    this.changeFilter();
  }

  checkEcts(which: number, isChecked: boolean) {
    this.chosenEcts[which] = isChecked;
    this.changeFilter();
  }

}
