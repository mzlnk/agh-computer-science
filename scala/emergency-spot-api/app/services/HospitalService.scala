package services

import javax.inject.{Inject, Singleton}
import model.dto.hospital.{AddressDto, HospitalDetailsDto, HospitalDto}
import model.dto.hospitalward.HospitalWardDto
import model.entity.{HospitalParams, HospitalWardParams}
import repositories.{HospitalRepository, HospitalWardRepository}

trait HospitalService {
  def findAll(params: HospitalParams): List[HospitalDto]

  def findOne(id: Long): Option[HospitalDetailsDto]
}

@Singleton
class HospitalServiceImpl @Inject()(hospitalRepository: HospitalRepository,
                                    hospitalWardRepository: HospitalWardRepository) extends HospitalService {

  override def findAll(params: HospitalParams): List[HospitalDto] = {
    hospitalRepository
      .findAll(params)
      .toStream
      .map(entity => HospitalDto(
        entity.id,
        entity.longitude,
        entity.latitude,
        entity.name,
        entity.description,
        AddressDto(
          entity.country,
          entity.city,
          entity.street,
          entity.streetNumber
        )
      ))
      .toList
  }

  override def findOne(id: Long): Option[HospitalDetailsDto] = {
    hospitalRepository
      .findById(id)
      .map(hospital => HospitalDetailsDto(
        hospital.id,
        hospital.name,
        hospital.description,
        hospital.longitude,
        hospital.latitude,
        AddressDto(
          hospital.country,
          hospital.city,
          hospital.street,
          hospital.streetNumber
        ),
        hospitalRepository.getAverageRating(hospital.id),
        hospitalWardRepository
          .findAll(HospitalWardParams(Option(List.empty[String]), None, None, Option(hospital.id)))
          .toStream
          .map(ward => HospitalWardDto(
            ward.id,
            HospitalDto(
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
            ),
            ward.wardType
          ))
          .toList
      ))
  }

}
