import React, { useState, ChangeEvent, KeyboardEvent } from "react";
import ReactDOM from "react-dom";
import styled from "styled-components";

import { M4 } from "../../math/geometry/index";
import { range } from "../../util";

const StyledApp = styled.div`
  text-align: center;
`;

const Grid = styled.div`
  display: inline-grid;
  grid-template-columns: 96px 96px 96px 96px;
  grid-template-rows   : 32px 32px 32px 32px;
`;

const Input = React.forwardRef((props: {
  onChage: (n: number) => void,
  onKeyDown: (direction: { x: number, y: number }) => void,
}, ref: React.Ref<HTMLInputElement>) => {

  const handleChange = (e: ChangeEvent) => {
    const val = (e.target as HTMLInputElement).value;
    if (!isNaN(+val)) props.onChage(+val);
  };

  const handleKeyDown = (e: KeyboardEvent) => {
    if (e.keyCode === 38) {
      props.onKeyDown({ x: 0, y: -1 });
    } else if (e.keyCode === 40) {
      props.onKeyDown({ x: 0, y: 1 });
    } else if (e.keyCode === 37) {
      const input = e.target as HTMLInputElement;
      const start = input.selectionStart;
      if (start === 0) {
        props.onKeyDown({ x: -1, y: 0 });
      }
    } else if (e.keyCode === 39) {
      const input = e.target as HTMLInputElement;
      const length = input.value.length;
      const start = input.selectionStart;
      if (start === length) {
        props.onKeyDown({ x: 1, y: 0 });
      }
    }
  };

  return (
    <input
      type="text"
      onChange={handleChange}
      onKeyDown={handleKeyDown}
      ref={ref}
    />
  );
});

const Output = (props: { value: number }) => (
  <input
    type="text"
    value={ isNaN(props.value) ? "" : props.value }
    readOnly={true}
    tabIndex={-1}
  />
);

const App = () => {
  const [src, setSrc] = useState(new Float32Array(range(16).map(_ => 1)));
  const [dst, setDst] = useState(new Float32Array(range(16).map(_ => NaN)));
  const [message, setMessage] = useState("");

  const inputs = range(16).map(_ => React.createRef<HTMLInputElement>());
  
  const changeInput = (n: number, index: number) => {
    const data = src.map((d, i) => i === index ? n : d);
    setSrc(data);

    const m = M4(data);
    if (m.determinant() === 0) {
      setMessage("逆行列が存在しません！");
      setDst(new Float32Array(range(16).map(_ => NaN)));
      return;
    }
    setMessage("");
    setDst(m.invert().primitive());
  };

  const keydownInput = (d: { x: number, y: number }, index: number) => {
    const idx = ((index + d.x + d.y * 4) + 16) % 16;
    inputs[idx].current.focus();
  };

  return (
    <StyledApp>
      <Grid>
        { range(16).map(i => {
          return <Input
            key={i}
            onChage={ n => changeInput(n, i) }
            onKeyDown={ d => keydownInput(d, i) }
            ref={inputs[i]}
          />
        }) }
      </Grid>
      <hr />
      <Grid>
        { range(16).map(i => <Output key={i} value={dst[i]} />) }
      </Grid>
      <div>{message}</div>
    </StyledApp>
  )
};

ReactDOM.render(
  <App />,
  document.getElementById("container"),
);
