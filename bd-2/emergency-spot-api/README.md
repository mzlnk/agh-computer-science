# Emergency Spot - REST API

Backend server with REST API provided for EmergencySpot App. Built with Spring Boot Web and secured with JWT authorization.

Documentation available here: https://docs.mzlnk.pl/emergencyspotapi

---

## About project

This application is a part of the project for classes *Databases 2* which take place on 2nd year of Computer Science at the faculty of Computer Science, Electronics and Telecomunications at AGH UST.

The other part of the project is user interface (Android app) which connects to the REST API and provides data manipulation support. Link to the repository: https://github.com/macko99/androidMapApp

Project authors:
- Maciej Kozub
- Marcin Zielonka

---

## Build

To build project:
- clone project from this repository
- fill credentials in `application.properties` file
- build `jar` file using command `mvn clean package`
- run `jar` file using command `java -jar emergency-spot-api-1.1.jar`

---

## Credentials

Before deploy app, you have to fill credentials in `application.properties` file:

```properties
server.port=${PORT:5000}

spring.jpa.database=mysql
spring.jpa.hibernate.ddl-auto=update
spring.jpa.properties.hibernate.dialect=org.hibernate.dialect.MySQL57InnoDBDialect
spring.datasource.url=jdbc:mysql://[host]:[port]/[db_name]
spring.datasource.username=[user]
spring.datasource.password=[password]

spring.jackson.serialization.indent_output=true

emergencyspotapi.jwt.access.token.validity=18000
emergencyspotapi.jwt.secret=[secret_key]
emergencyspotapi.jwt.token.prefix=Bearer\u0020
emergencyspotapi.jwt.header=Authorization
emergencyspotapi.jwt.authorities.key=scopes
```

---

## REST API - endpoints:

* [/hospitals GET](#list-hospitals)
* [/hospitals/{id} GET](#find-one-hospital)
* [/hospitals/{id}/wards GET](#find-hospital-wards)
* [/hospitals/nearest GET](#find-nearest-hospital)
+ [/wards GET](#list-wards)
+ [/wards/{id} GET](#find-one-ward)
+ [/wards/{id}/reviews GET](#find-ward-reviews)
+ [/wards/{id}/stays GET](#find-ward-hospital-stays)
+ [/wards POST](#create-new-hospital-ward)
+ [/wards/{id} DELETE](#delete-existing-hospital-ward)
* [/reviews GET](#list-reviews)
* [/reviews/{id} GET](#find-one-review)
* [/reviews POST](#create-review)
* [/reviews/{id} PUT](#update-review)
* [/reviews/{id} DELETE](#delete-review)
+ [/stays GET](#list-hospital-stays)
+ [/stays/{id} GET](#find-one-hospital-stay)
+ [/stays/me GET](#find-user-hospital-stays)
+ [/stays POST](#create-hospital-stay)
+ [/stays DELETE](#delete-hospital-stay)
* [/users/signup POST](#sign-up)
* [/token/generate POST](#obtain-token)

### List hospitals

Returns all hospitals matching to applied filters

`GET /hospitals`

Request params:

| Param       | Description                         | Type            | Required |
| ----------- | ----------------------------------- | --------------- | -------- |
| `name`      | hospital name                       | `String`        | *No*     |
| `longitude` | longitude where hospital is located | `Double`        | *No*     |
| `latitude`  | latitude where hospital is located  | `Double`        | *No*     |
| `country`   | country where hospital is located   | `String`        | *No*     |
| `city`      | city where hospital is located      | `String`        | *No*     |
| `wards`     | wards which are located in hospital | `Array<String>` | *No*     |

Usage:

+ `GET /hospitals`
+ `GET /hospitals?name=X`
+ `GET /hospitals?name=X&city=Y&longitude=30.4516&wards=ICU,ER`

---

### Find one hospital

Returns hospital with given ID.

`GET /hospitals/{id}`

+ `id` - hospital ID

Usage:

+ `GET /hospitals/981341`
+ `GET /hospitals/24141`

---

### Find hospital wards

Returns hospital wards associated with hospital with given ID

`GET /hospitals/{id}/ward`

+ `id` - hospital ID

Usage:

+ `GET /hospitals/981341/wards`
+ `GET /hospitals/24141/wards`

---

### Find nearest hospital

Returns hospital nearest given longitude and latitude.

`GET /hospitals/nearest`

Request params:

| Param       | Description                     | Type            | Required |
| ----------- | ------------------------------- | --------------- | -------- |
| `longitude` | longitude where user is located | `Double`        | *Yes*    |
| `latitude`  | latitude where user is located  | `Double`        | *Yes*    |

Usage:

+ `GET /hospitals/nearest?longitude=20.8391&latitude=19.2003`

---

### List wards

Returns all hospital wards matching to applied filters

`GET /wards`

Request params:

| Param           | Description                         | Type            | Required |
| --------------- | ----------------------------------- | --------------- | -------- |
| `wards`         | hospital ward types                 | `Array<String>` | *No*     |
| `min_capacity`  | minimum hospital ward capacity      | `Double`        | *No*     |
| `max_capacity`  | maximum hospital capacity           | `Double`        | *No*     |
| `hospital`      | hospital ID                         | `Long`          | *No*     |

Usage:

+ `GET /wards`
+ `GET /wards?min_capacity=20`
+ `GET /wards?max_capacity=50&wards=ICU,ER

---

### Find one ward

Returns hospital ward with given ID.

`GET /wards/{id}`

+ `id` - hospital ward ID

Usage:

+ `GET /wards/90111298`
+ `GET /wards/5993210`

---

### Find ward reviews

Returns list of hospital reviews associated with ward with given ID

`GET /wards/{id}/reviews`

+ `id` - hospital ward ID

Usage:

+ `GET /wards/90111298/reviews`
+ `GET /wards/5993210/reviews`

---

### Find ward hospital stays

Returns list of hospital stays associated with ward with given ID

`GET /wards/{id}/stays`

+ `id` - hospital ward ID

Usage:

+ `GET /wards/90111298/stays`
+ `GET /wards/5993210/stays`

---

### Create new hospital ward

Create new hospital ward based on given details

`POST /wards`

Request body:

```{json}
{
  hospitalId: [hospital_id],
  wardType: [ward_type],
  capacity: [capacity]
}
```

| Param           | Description                         | Type            | Required |
| --------------- | ----------------------------------- | --------------- | -------- |
| `hospitalId`    | hospital associated with new ward   | `Long`          | *Yes*    |
| `wardType`      | ward type                           | `String`        | *Yes*    |
| `capacity`      | ward capacity                       | `Double`        | *Yes*    |

Usage:

+ `POST /wards`

---

### Delete existing hospital ward

Delete existing hospital ward by given ID.

`DELETE /wards/{id}`

+ `id` - hospital ward ID

Usage:

+ `DELETE /wards/90111298`
+ `DELETE /wards/5993210`

---

### List reviews

Return all reviews matching to applied filters.

`GET /reviews`

Request params:

| Param           | Description                         | Type            | Required |
| --------------- | ----------------------------------- | --------------- | -------- |
| `min_rating`    | minimum review rating               | `Double`        | *No*     |
| `max_rating`    | maximum review rating               | `Double`        | *No*     |
| `hospital`      | hospital ID associated with review  | `Long`          | *No*     |

Usage:

+ `GET /reviews`
+ `GET /reviews?min_rating=4.5`
+ `GET /reviews?min_rating=8.0&hospital=9013314`

---

### Find one review

Return review with given ID.

`GET /reviews/{id}`

+ `id` - review ID

Usage:

+ `GET /review/71401298`
+ `GET /review/3613210`

---

### Create review

Create new review

`POST /reviews`

Request body:

```json
{
  "hospitalStayId": [hospitalStayId],
  "rating": [rating],
}
```

| Param            | Description                           | Type            | Required |
| ---------------- | ------------------------------------- | --------------- | -------- |
| `hospitalStayId` | hospital stay associated with review  | `Long`          | *Yes*    |
| `rating`         | review rating                         | `Double`        | *Yes*    |

---

### Update review

Updates exisiting review.

`PUT /reviews`

Request body:

```json
{
  "hospitalStayId": [hospitalStayId],
  "rating": [rating],
}
```

| Param              | Description                         | Type            | Required |
| ------------------ | ----------------------------------- | --------------- | -------- |
| `hospitalReviewId` | rhospital review ID                 | `Long`          | *Yes*    |
| `newRating`        | new review rating                   | `Double`        | *Yes*    |

Usage:

+ `PUT /reviews`

---

### Delete review

Deletes review with given ID

`DELETE /reviews/{id}`

+ `id` - review ID

Usage:

+ `DELETE /reviews/1748391`
+ `DELETE /reviews/6382649001`

---

### List hospital stays

Return all hospital stays matching to applied filters.

`GET /stays`

Request params:

| Param           | Description                                                  | Type        | Required | Format       |
| --------------- | ------------------------------------------------------------ | ------------| -------- | ------------ |
| `date_from`     | start date range                                             | `Calendar`  | *No*     | *dd-MM-yyyy* |
| `date_to`       | end date range                                               | `Calendar`  | *No*     | *dd-MM-yyyy* |
| `ward`          | hospital ward's ID where hospital stay is associated with    | `Long`      | *No*     |              |
| `patient`       | hospital patient's ID where hospital stay is associated with | `Long`      | *No*     |              |

Usage:

+ `GET /stays`
+ `GET /stays?date_from=01-01-2020`
+ `GET /stays?ward=1&patient=18314`

---

### Find one hospital stay

Return hospital stay with given ID.

`GET /stay/{id}`

+ `id` - hospital stay ID

Usage:

+ `GET /stays/71401298`
+ `GET /stays/3613210`

---

### Find user hospital stays

Return hospital stays associated with user.

`GET /stay/me`

Usage:

+ `GET /stays/me`

---

### Create hospital stay

Create new hospital stay

`POST /stays`

Request body:

```json
{
  hospitalWardId: [hospital_ward_id],
  patientId: [patient_id],
  dateFrom: [date_from],
  dateTo: [date_to]
}
```

| Param            | Description                           | Type            | Required | Format       |
| ---------------- | ------------------------------------- | --------------- | -------- | ------------ |
| `hospitalWardId` | hospital ward associated with stay    | `Long`          | *Yes*    | *dd-MM-yyyy* |
| `patientId`      | patient associated with stay          | `Long`          | *Yes*    |              |
| `dateFrom`       | hospital stay start date              | `Calendar`      | *Yes*    | *dd-MM-yyyy* |
| `dateTo`         | hospital stay end date                | `Calendar`      | *Yes*    | *dd-MM-yyyy* |

---

### Delete hospital stay

Deletes hospital stay with given ID

`DELETE /stays/{id}`

+ `id` - hospital stay ID

Usage:

+ `DELETE /stays/1748391`
+ `DELETE /stays/6382649001`

---

### Sign up

Creates new user with provided details

`POST /users/signup`

Request body:

```json
{
  username: [username],
  firstName: [first_name],
  lastName: [last_name],
  pesel: [pesel],
  password: [password]
}
```

---

### Obtain token

Obtain JWT token based on provided authentication details

`POST /token/generate`

Request body:

```json
{
  username: [username],
  password: [password]
}
```
