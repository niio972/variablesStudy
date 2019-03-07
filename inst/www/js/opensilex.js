function initOpenSilexConnection(){
    var params = new window.URLSearchParams(window.location.search);
    var config = {};
    config.wsUrl = params.get("wsUrl");
    config.token = params.get("access_token");
    return config;
}