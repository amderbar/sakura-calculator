import { babel } from '@rollup/plugin-babel';
import commonjs from '@rollup/plugin-commonjs';
import { nodeResolve } from '@rollup/plugin-node-resolve';

export default {
    input: 'dist/sakura-calculator.js',
    output: {
        file: 'dist/sakura-calculator.min.js',
        format: 'es'
    },
    plugins: [
        nodeResolve(),
        commonjs(),
        babel({ babelHelpers: 'bundled' })
    ]
};
