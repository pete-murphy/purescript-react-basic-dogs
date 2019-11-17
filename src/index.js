import React from "react";
import ReactDOM from "react-dom";
import { mkApp } from "./output/App";

import "./index.scss";

const App = mkApp();

ReactDOM.render(<App />, document.getElementById("root"));
