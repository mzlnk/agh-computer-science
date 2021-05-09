package pl.mzlnk.emergencyspot.service.network.requests;

import com.android.volley.Request;
import com.google.gson.reflect.TypeToken;

import java.util.List;

import lombok.AllArgsConstructor;
import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;
import pl.mzlnk.emergencyspot.model.hospital.HospitalParamsDto;

@AllArgsConstructor
public class RetrieveHospitalsHttpRequestParams extends BaseHttpRequestParams<HospitalDto> {

    private HospitalParamsDto hospitalParams;

    @Override
    public int getRequestMethod() {
        return Request.Method.GET;
    }

    @Override
    public String getUrl() {
        return "http://192.168.0.21:5000/hospitals/";
    }

    @Override
    public TypeToken<List<HospitalDto>> receivedDataTypeToken() {
        return new TypeToken<List<HospitalDto>>() {};
    }

}
