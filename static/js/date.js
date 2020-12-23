function localizeDateTimes(dts){
  dts.forEach((dateEl) => {
    let utcTime = dateEl.getAttribute("datetime");
    if(utcTime){
      let parsedDate = new Date(utcTime);
      dateEl.textContent = parsedDate.toLocaleString();
    }
  });
}

function localizeDates(ds){
  ds.forEach((dateEl) => {
    let utcTime = dateEl.getAttribute("datetime");
    if(utcTime){
      let parsedDate = new Date(utcTime);
      dateEl.textContent = parsedDate.toLocaleDateString();
    }
  });
}

function navigateToMoment(event){
  event.preventDefault();

  let query = new URLSearchParams(window.location.search);
  let currentMoment = new Date();
  query.set("at", currentMoment.toISOString());
  
  let newLocation = new URL(`/transits?${query.toString()}`, window.location.origin);
  window.location.assign(newLocation.toString());
}

const localDateTimes = document.querySelectorAll(".local-datetime");
const localDates = document.querySelectorAll(".local-date");
const momentLink = document.querySelector("#moment-link");

window.onload = () => {
  localizeDateTimes(localDateTimes);
  localizeDates(localDates);
  momentLink.classList.remove("d-invisible");
};
momentLink.addEventListener('click', navigateToMoment);
