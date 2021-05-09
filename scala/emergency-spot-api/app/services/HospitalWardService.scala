package services

import javax.inject.{Inject, Singleton}
import model.dto.hospital.{AddressDto, HospitalDto}
import model.dto.hospitalward.{HospitalWardDetailsDto, HospitalWardDto}
import model.entity.HospitalWardParams
import repositories.{HospitalRepository, HospitalWardRepository}

trait HospitalWardService {
  def findAll(params: HospitalWardParams): List[HospitalWardDto]
  def findOne(id: Long): Option[HospitalWardDetailsDto]
}

@Singleton
class HospitalWardServiceImpl @Inject()(hospitalWardRepository: HospitalWardRepository,
                                        hospitalRepository: HospitalRepository) extends HospitalWardService {

  override def findAll(params: HospitalWardParams): List[HospitalWardDto] = {
    hospitalWardRepository
      .findAll(params)
      .toStream
      .map(ward => HospitalWardDto(
        ward.id,
        getHospitalDtoFromId(ward.hospitalId),
        ward.wardType
      ))
      .toList
  }

  override def findOne(id: Long): Option[HospitalWardDetailsDto] = {
    hospitalWardRepository
      .findById(id)
      .map(ward => HospitalWardDetailsDto(
        ward.id,
        getHospitalDtoFromId(ward.hospitalId),
        ward.wardType,
        ward.capacity,
        hospitalWardRepository.getCurrentHospitalStays(ward.id),
        hospitalWardRepository.getAverageRating(ward.id)
      ))
  }

  private def getHospitalDtoFromId(id: Long) = {
    hospitalRepository
      .findById(id)
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
      .orNull
  }

}
