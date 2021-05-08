export interface Course {
  id: string;
  name: string;
  description: string;
  url: string;
  ects: number;
  semester: number;
  courseType: CourseType;
  attendeesLimit: number;
  attendees: number;
  rates: number;
  rateSum: number;
}

export enum CourseType {
  Lecture,
  Practicals,
  Lab,
  Project
}
