import resolve from 'rollup-plugin-node-resolve';
import babel from 'rollup-plugin-babel'
import commonjs from 'rollup-plugin-commonjs';
import { uglify } from 'rollup-plugin-uglify';
import replace from 'rollup-plugin-replace';

// `npm run build` -> `production` is true
// `npm run dev` -> `production` is false
const production = !process.env.ROLLUP_WATCH;

export default {
	input: 'src/main.js',
	output: {
		file: '../static/js/bundle.js',
		format: 'iife', // immediately-invoked function expression — suitable for <script> tags
		// format: 'iife', // immediately-invoked function expression — suitable for <script> tags
		sourcemap: true
	},
	plugins: [
        resolve(), // tells Rollup how to find date-fns in node_modules
		commonjs({
            include: 'node_modules/**'
        }), // converts date-fns to ES modules
        babel(),
        replace({
            'process.env.NODE_ENV': JSON.stringify( 'production' )
        }),
		production && uglify() // minify, but only in production
	]
};