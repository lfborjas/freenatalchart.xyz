function initGeolocation() {
  let el  = document.getElementById('geocode-city-autocomplete');
  let lat = document.getElementById('lat');
  let lng = document.getElementById('lng');
  let err = document.getElementById('err');
  let errMsg = document.getElementById('errMsg');
  let btn = document.querySelector(".btn-primary");

  el.addEventListener('citySelected', function (e) {
    lat.value = e.detail.latitude;
    lng.value = e.detail.longitude;
  });

  // TODO: capture the input event for the autocomplete's input?
  /*placesAutocomplete.on('clear', function () {
    lat.value = '';
    lng.value = '';
  });*/

  el.addEventListener('lookupError', function (e) {
    err.classList.remove("d-none");
    errMsg.textContent = "Looks like our location service is temporarily unavailable. Please try again in a little bit. If the problem persists, please submit an issue."
    btn.setAttribute("disabled", true);
  });
}

function currentDateComponents() {
  let now = new Date();
  let day = now.getDate();
  let month = now.getMonth() + 1;
  let year = now.getFullYear();
  let hour24 = now.getHours();
  let hour = hour24 % 12 || 12;
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

function initChartOfTheMomentLink(){
  let chartOfTheMomentLink = document.getElementById('chart-of-the-moment');
  let dayParts = currentDateComponents();

  chartOfTheMomentLink.textContent = "Chart of the moment in NYC";
  chartOfTheMomentLink.href = "/full-chart?location=New+York&month=" + dayParts.month
    + "&day=" + dayParts.day
    + "&year=" + dayParts.year
    + "&hour=" + dayParts.hour
    + "&minute=" + dayParts.minute
    + "&day-part=" + dayParts.partOfDay
    + "&lat=40.6815&lng=-73.8365";
  
  navigator.geolocation.getCurrentPosition(
    function (pos) {
      chartOfTheMomentLink.textContent = "Chart of the moment for your location";
      chartOfTheMomentLink.href = "/full-chart?location=Your+Location&month=" + dayParts.month
        + "&day=" + dayParts.day
        + "&year=" + dayParts.year
        + "&hour=" + dayParts.hour
        + "&minute=" + dayParts.minute
        + "&day-part=" + dayParts.partOfDay
        + "&lat=" + pos.coords.latitude
        + "&lng=" + pos.coords.longitude;      
    });

  chartOfTheMomentLink.classList.remove("d-invisible");
}

window.onload = () => {
  initChartOfTheMomentLink();
}
