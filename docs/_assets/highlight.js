document.addEventListener("DOMContentLoaded", () => {
  const marks = document.querySelectorAll("mark");

  const observer = new IntersectionObserver(
    (entries, observer) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.classList.add("animate");
          // observer.unobserve(entry.target); // animate only once
        }
      });
    },
    {
      threshold: 0.4 // % of element visible before triggering
    }
  );

  marks.forEach(mark => observer.observe(mark));
});
