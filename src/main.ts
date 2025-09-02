import { match } from "ts-pattern";
import { Elm, type FromElm } from "@/Main.elm";

const basePath = import.meta.env.BASE_URL;
const theme = localStorage.getItem("theme") || "dark";
document.querySelector("html")?.setAttribute("data-theme", theme);
const node = document.getElementById("app");
const {
  ports: {
    interopToElm: { send },
    interopFromElm: { subscribe },
  },
} = Elm.Main.init({
  node,
  flags: {
    basePath,
    theme,
  },
});

subscribe((m: FromElm) =>
  match(m)
    .with({ tag: "ElmReady" }, () => send({ tag: "JSReady" }))
    .with({ tag: "SaveTheme" }, ({ theme }) => {
      localStorage.setItem("theme", theme);
      document.querySelector("html")!.setAttribute("data-theme", theme);
    })
    .exhaustive()
);
