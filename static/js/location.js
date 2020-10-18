function initGeolocation(appId, appKey) {
  let placesAutocomplete = places({
    appId: appId,
    apiKey: appKey,
    container: document.getElementById('location')
  }).configure({
    type: 'city',
    aroundLatLngViaIP: false,
  });

  let lat = document.getElementById('lat');
  let lng = document.getElementById('lng');
  let err = document.getElementById('err');
  let errMsg = document.getElementById('errMsg');
  let btn = document.querySelector(".btn-primary");

  placesAutocomplete.on('change', function (e) {
    lat.value = e.suggestion.latlng.lat;
    lng.value = e.suggestion.latlng.lng;
  });

  placesAutocomplete.on('clear', function () {
    lat.value = '';
    lng.value = '';
  });

  placesAutocomplete.on('error', function (e) {
    err.classList.remove("d-none");
    errMsg.textContent = "Looks like our location service is currently unreachable.";
    btn.setAttribute("disabled", true);
  });

  placesAutocomplete.on('limit', function (e) {
    err.classList.remove("d-none");
    errMsg.textContent = "Looks like our location service is temporarily unavailable. Please try again in a little bit. If the problem persists, please submit an issue."
    btn.setAttribute("disabled", true);
  });

  // optional behavior for the "chart of the moment" stuff
  // note that this is only available in secure contexts!
  function currentDateComponents() {
    let now = new Date();
    let day = now.getDate();
    let month = now.getMonth() + 1;
    let year = now.getFullYear();
    let hour24 = now.getHours();
    let hour = hour24 % 12;
    let partOfDay = hour24 > 12 ? "pm" : "am";
    let minute = now.getMinutes();
    return {
      day: day,
      month: month,
      year: year,
      hour: hour,
      minute: minute,
      partOfDay: partOfDay
    };
  }

  let chartOfTheMomentLink = document.getElementById('chart-of-the-moment');
  navigator.geolocation.getCurrentPosition(
    function (pos) {
      let dayParts = currentDateComponents();
      chartOfTheMomentLink.textContent = "Or see the chart of the moment for your location";
      chartOfTheMomentLink.href = "/full-chart?location=Your+Location&month=" + dayParts.month
        + "&day=" + dayParts.day
        + "&year=" + dayParts.year
        + "&hour=" + dayParts.hour
        + "&minute=" + dayParts.minute
        + "&day-part=" + dayParts.partOfDay
        + "&lat=" + pos.coords.latitude
        + "&lng=" + pos.coords.longitude;      
    },
    function (e) {
      ///full-chart?location=Queens&month=10&day=16&year=2020&hour=6&minute=36&day-part=pm&lat=40.6815&lng=-73.8365
      let dayParts = currentDateComponents();
      chartOfTheMomentLink.textContent = "Or see an example chart of the moment";
      chartOfTheMomentLink.href = "/full-chart?location=New+York&month=" + dayParts.month
        + "&day=" + dayParts.day
        + "&year=" + dayParts.year
        + "&hour=" + dayParts.hour
        + "&minute=" + dayParts.minute
        + "&day-part=" + dayParts.partOfDay
        + "&lat=40.6815&lng=-73.8365";
    })
}
