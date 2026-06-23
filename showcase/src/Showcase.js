export const setThemeAttr = (t) => () => {
  document.documentElement.setAttribute("data-theme", t);
};

export const prefersDark = () =>
  typeof window !== "undefined" &&
  window.matchMedia &&
  window.matchMedia("(prefers-color-scheme: dark)").matches;
