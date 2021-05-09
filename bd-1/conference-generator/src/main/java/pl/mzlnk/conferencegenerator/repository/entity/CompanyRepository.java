package pl.mzlnk.conferencegenerator.repository.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Company;

class CompanyRepository extends BaseEntityRepository<Company> {

    CompanyRepository() {
        super(EntityType.COMPANY);
    }

}
