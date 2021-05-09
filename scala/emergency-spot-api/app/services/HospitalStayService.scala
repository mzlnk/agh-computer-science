package services

import javax.inject.{Inject, Singleton}
import model.dto.hospital.{AddressDto, HospitalDto}
import model.dto.hospitalstay.{HospitalStayDetailsDto, HospitalStayDto, NewHospitalStayDto}
import model.dto.hospitalward.HospitalWardDto
import model.entity.HospitalStayParams
import repositories.{HospitalRepository, HospitalStayRepository, HospitalWardRepository}

trait HospitalStayService {
  def findAll(params: HospitalStayParams): List[HospitalStayDto]

  def findOne(id: Long): Option[HospitalStayDetailsDto]

  def create(stay: NewHospitalStayDto): HospitalStayDetailsDto
}

@Singleton
class HospitalStayServiceImpl @Inject()(hospitalStayRepository: HospitalStayRepository,
                                        hospitalWardRepository: HospitalWardRepository,
                                        hospitalRepository: HospitalRepository) extends HospitalStayService {

  override def findAll(params: HospitalStayParams): List[HospitalStayDto] = {
    hospitalStayRepository
      .findAll(params)
      .toStream
      .map(entity => HospitalStayDto(
        entity.id,
        getHospitalWardDtoFromId(entity.hospitalWardId),
        entity.dateFrom,
        entity.dateTo
      ))
      .toList
  }

  override def findOne(id: Long): Option[HospitalStayDetailsDto] = {
    hospitalStayRepository
      .findById(id)
      .map(entity => HospitalStayDetailsDto(
        entity.id,
        getHospitalWardDtoFromId(entity.hospitalWardId),
        entity.dateFrom,
        entity.dateTo
      ))
  }

  override def create(stay: NewHospitalStayDto): HospitalStayDetailsDto = {
    val newStay = hospitalStayRepository.create(stay)

    HospitalStayDetailsDto(
      newStay.id,
      getHospitalWardDtoFromId(newStay.hospitalWardId),
      newStay.dateFrom,
      newStay.dateTo
    )
  }

  private def getHospitalWardDtoFromId(id: Long) = {
    hospitalWardRepository
      .findById(id)
      .map(ward => HospitalWardDto(
        ward.id,
        hospitalRepository
          .findById(ward.hospitalId)
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
        ward.wardType
      ))
      .orNull
  }

}
