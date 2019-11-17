import React from "react";
import ReactDOM from "react-dom";
import { mkApp } from "./output/App";

const App = mkApp();

ReactDOM.render(<App />, document.getElementById("root"));
