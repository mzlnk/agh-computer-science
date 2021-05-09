package services

import javax.inject.{Inject, Singleton}
import model.dto.hospital.{AddressDto, HospitalDto}
import model.dto.hospitalreview.{HospitalReviewDetailsDto, HospitalReviewDto, NewHospitalReviewDto, UpdateHospitalReviewDto}
import model.dto.hospitalward.HospitalWardDto
import model.entity.HospitalReviewParams
import repositories.{HospitalRepository, HospitalReviewRepository, HospitalWardRepository}

trait HospitalReviewService {
  def findAll(params: HospitalReviewParams): List[HospitalReviewDto]

  def findOne(id: Long): Option[HospitalReviewDetailsDto]

  def create(review: NewHospitalReviewDto): HospitalReviewDetailsDto

  def update(review: UpdateHospitalReviewDto): HospitalReviewDetailsDto
}

@Singleton
class HospitalReviewServiceImpl @Inject()(hospitalReviewRepository: HospitalReviewRepository,
                                          hospitalWardRepository: HospitalWardRepository,
                                          hospitalRepository: HospitalRepository) extends HospitalReviewService {

  override def findAll(params: HospitalReviewParams): List[HospitalReviewDto] = {
    hospitalReviewRepository.findAll(params)
      .toStream
      .map(entity => HospitalReviewDto(entity.id, entity.rating))
      .toList
  }

  override def findOne(id: Long): Option[HospitalReviewDetailsDto] = {
    hospitalReviewRepository
      .findById(id)
      .map(entity => {
        val ward = getHospitalWardDtoFromId(entity.hospitalWardId)
        HospitalReviewDetailsDto(id, entity.rating, ward)
      })
  }

  override def create(review: NewHospitalReviewDto): HospitalReviewDetailsDto = {
    val newReview = hospitalReviewRepository.create(review)
    println("ward id: " + newReview.hospitalWardId)
    val ward = getHospitalWardDtoFromId(newReview.hospitalWardId)

    HospitalReviewDetailsDto(newReview.id, newReview.rating, ward)
  }

  override def update(review: UpdateHospitalReviewDto): HospitalReviewDetailsDto = {
    val updatedReview = hospitalReviewRepository.update(review)
    val ward = getHospitalWardDtoFromId(updatedReview.hospitalWardId)

    HospitalReviewDetailsDto(updatedReview.id, updatedReview.rating, ward)
  }

  private def getHospitalWardDtoFromId(id: Long) = {
    hospitalWardRepository
      .findById(id)
      .map(entity => HospitalWardDto(
        entity.id,
        hospitalRepository
          .findById(entity.hospitalId)
          .map(hospital => HospitalDto(
            hospital.id,
            hospital.longitude,
            hospital.latitude,
            hospital.name,
            hospital.description,
            AddressDto(
              hospital.country,
              hospital.city,
              hospital.street,
              hospital.streetNumber
            )
          ))
          .orNull,
        entity.wardType
      ))
      .orNull
  }

}
