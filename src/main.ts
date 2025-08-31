import { match } from "ts-pattern";
import { Elm, type FromElm } from "@/Main.elm";

const basePath = import.meta.env.BASE_URL;
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
  },
});

subscribe((m) =>
  match<FromElm>(m)
    .with({ tag: "ElmReady" }, () => send({ tag: "JSReady" }))
    .exhaustive()
);
