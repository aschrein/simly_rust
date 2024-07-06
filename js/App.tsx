import React, { useCallback, useEffect, useState } from "react";
import { GoldenLayout, JsonValue, LayoutConfig, ComponentContainer } from 'golden-layout';
import ReactDOM from 'react-dom';
import AceEditor from 'react-ace';
// import '../static/index.css';
import 'brace/theme/tomorrow_night_eighties';
import 'brace/mode/assembly_x86';
import 'brace/mode/c_cpp';

type WasmInstance = typeof import("../pkg/index.js");

var globals: any = {};

class TextEditorComponent extends React.Component<{}, {}> {

	text: string = "";

	constructor(props: any, context: any) {
		super(props, context);

		this.onChange = this.onChange.bind(this);
		this.onResize = this.onResize.bind(this);
		this.state = {};
		this.text = "";

	}

	onResize() {
	}

	onChange(newValue: string) {
		this.text = newValue;
	}
	componentDidMount() {
	}
	render() {

		return (
			<div className="ace_editor_container">
				<AceEditor
					value={this.text}
					ref="editor"
					mode="c_cpp"
					theme="tomorrow_night_eighties"
					onChange={this.onChange}
					name="UNIQUE_ID_OF_DIV"
					editorProps={{
						$blockScrolling: true
					}}
					wrapEnabled={false}
					height="700px"
					width="512px"
				/>
			</div>
		);
	}
}

class GoldenLayoutWrapper extends React.Component {

	globals: any = {};
	layout: any = null;


	constructor(props: any, context: any) {
		super(props, context);
	}

	componentDidMount() {
		this.globals = {};

		// Build basic golden-layout config
		const config: LayoutConfig = {
			root: {
				type: 'row',
				content: [{
					type: 'component',
					componentType: 'TextEditor',
					componentName: 'TextEditor',
					title: 'TextEditor'
				},
				]
			}
		};

		var layout = new GoldenLayout(config, this.layout);

		this.layout = layout;
		layout.registerComponentFactoryFunction('TextEditor',
			(container, state) => {
				return ReactDOM.render(
					React.createElement(TextEditorComponent),
					container.getElement());
			}
		);


		window.React = React;
		window.ReactDOM = ReactDOM;
		window.addEventListener('resize', () => {
			// layout.updateSize();
		});
		layout.init();

		//layout.updateSize();
	}

	render() {
		return (
			<div className='goldenLayout'
				ref={input => this.layout = input} />
		);
	}
}

function App() {
	globals = {};
	const initialize = useCallback(async () => {
		try {
			globals.wasm = await import("../pkg/index.js");
			globals.wasm.test();

		} catch (error: any) {
			console.error(error);
		} finally {
			console.log("Wasm initialized");
		}
	}, []);
	useEffect(() => {
		async function loadWasm() {
			await initialize();
		}
		loadWasm();
	}, []);

	return <GoldenLayoutWrapper></GoldenLayoutWrapper>;
}

export default App;
