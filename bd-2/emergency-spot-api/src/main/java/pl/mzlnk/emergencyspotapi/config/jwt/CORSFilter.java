package pl.mzlnk.emergencyspotapi.config.jwt;

import javax.servlet.*;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class CORSFilter implements Filter {

    /**
     * Bean used to provide proper configuration for JWT token support
     * @param req servlet request
     * @param res servlet response
     * @param chain filter
     * @throws IOException .
     * @throws ServletException .
     */
    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {
        HttpServletResponse response = (HttpServletResponse) res;
        response.setHeader("Access-Control-Allow-Origin", "*");
        response.setHeader("Access-Control-Allow-Credentials", "true");
        response.setHeader("Access-Control-Allow-Methods", "POST, GET, PUT, OPTIONS, DELETE");
        response.setHeader("Access-Control-Max-Age", "3600");
        response.setHeader("Access-Control-Allow-Headers", "X-Requested-With, Content-Type, Authorization, Origin, Accept, Access-Control-Request-Method, Access-Control-Request-Headers");

        chain.doFilter(req, res);
    }

    /**
     * Method used to initialize filter
     * @param filterConfig filter config
     */
    @Override
    public void init(FilterConfig filterConfig) {}

    /**
     * Method used to cleanup
     */
    @Override
    public void destroy() {

    }

}
