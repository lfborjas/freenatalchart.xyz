function localizeDates(_event){
  localDates.forEach((dateEl) => {
    let utcTime = dateEl.getAttribute("datetime");
    if(utcTime){
      let parsedDate = new Date(utcTime);
      dateEl.textContent = parsedDate.toLocaleString();
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

const localDates = document.querySelectorAll(".local-datetime");
const momentLink = document.querySelector("#moment-link");

window.onload = localizeDates;
momentLink.addEventListener('click', navigateToMoment);
