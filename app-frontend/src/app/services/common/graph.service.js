/* globals console */
import _ from 'lodash';
import {Map, Set} from 'immutable';
import * as d3 from 'd3';
import $ from 'jquery';

class D3Element {
    constructor(el, id, options, $q) {
        this.el = el;
        this.id = id;
        this.options = options;
        this.$q = $q;
        this.callbacks = new Map();

        this.options = Object.assign({
            style: {
                height: '120px'
            },
            height: 120,
            width: this.el.clientWidth || 100,
            margin: {
                top: 5,
                bottom: 5,
                left: 0,
                right: 0
            }
        }, options);

        this.initSvg();
    }


    initSvg() {
        $(this.el).css({
            height: this.options.style.height,
            width: '100%'
        });
    }

    setData(data) {
        this.data = data;
        this.update();
        return this;
    }

    update() {
        this.render();
    }

    render() {
        this.callbacks.forEach((callback) => {
            callback();
        });

        console.log('rendering D3Element');
    }
}

class SinglebandHistogram extends D3Element {
    constructor(el, id, options, $q) {
        super(el, id, options, $q);
    }

    update() {
        // update all variables related to rendering and render
        super.update();
    }

    render() {
        // render d3 el
        console.log('rendering single band histogram');

        let svg = d3.select(this.el);
        let defs = svg.select('defs');
        if (!defs[0] || !defs[0].length) {
            defs = svg.append('defs');
        }

        if (this.data && this.data.histogram && this.data.breakpoints) {
            console.log('rendering with data:', this.data, this.options);

            this.calculateAxis(svg, defs);
            this.renderData(svg, defs);
            this.renderGradient(svg, defs);

        } else {
            console.log('Render called with no data');
        }

        super.render();
    }

    calculateAxis(svg, defs) {
        console.log('rendering axis', this.options);
        const xRange = [this.options.margin.left,
                        this.el.clientWidth - this.options.margin.right];
        console.log('range = ', xRange);
        this.xScale = d3.scaleLinear()
            .domain([d3.min(this.data.histogram, d => d.x),
                     d3.max(this.data.histogram, d => d.x)])
            .range(xRange);

        this.yScale = d3.scaleLinear()
            .domain([0, d3.max(this.data.histogram, d => d.y)])
            .nice()
            .range([this.options.height - this.options.margin.bottom,
                    this.options.margin.top]);
    }

    renderData(svg, defs) {
        // let line = d3.line()
        //     .x(v => this.xScale(v.x))
        //     .y(v => this.yScale(v.y))
        //     .curve(d3.curveStepAfter);
        // svg.append('path')
        //     .datum(this.data.histogram)
        //     .attr('fill', 'none')
        //     .attr('stroke', 'steelblue')
        //     .attr('stroke-width', '1.5')
        //     .attr('d', line);


        let area = d3.area()
            .x(v => this.xScale(v.x))
            .y0(this.options.height - this.options.margin.bottom)
            .y1(v => this.yScale(v.y))
            .curve(d3.curveStepAfter);

        svg.append('path')
            .data([this.data.histogram])
            .attr('class', 'data-fill')
            .attr('d', area);
    }

    renderGradient(svg, defs) {
        let linearGradient = defs.selectAll('linearGradient');
        if (!linearGradient[0] || !linearGradient[0].length) {
            linearGradient = defs.append('linearGradient');
        }

        let colorData = this.calculateGradientColors();

        linearGradient.attr('id', `line-gradient-${this.id}`)
            .attr('gradientUnits', 'userSpaceOnUse')
            .attr('x1', '0%').attr('y1', 0)
            .attr('x2', '100%').attr('y2', 0)
            .selectAll('stop')
            .data(colorData)
            .enter().append('stop')
            .attr('offset', (d) => d.offset)
            .attr('stop-color', (d) => d.color)
            .attr('stop-opacity', (d) => Number.isFinite(d.opacity) ? d.opacity : 1.0);
    }

    calculateGradientColors() {
        let max = d3.max(this.data.histogram, d => d.x);
        let min = d3.min(this.data.histogram, d => d.x);
        let range = max - min;
        let data = this.data.breakpoints.map((bp) => {
            let offset = (bp.value - min) / range * 100;
            return {offset, color: bp.color};
        }).sort((a, b) => a.offset - b.offset).map((bp) => {
            return {offset: `${bp.offset}%`, color: bp.color};
        });

        if (_.get(this.options, 'masks.min') || this.options.discrete) {
            let last = _.last(data);
            if (last.color === 'NODATA' || !this.options.discrete) {
                data.splice(0, 0, {offset: data[0].offset, color: '#353C58'});
                data.splice(0, 0, {offset: data[0].offset, color: '#353C58'});
            } else {
                data.splice(0, 0, {offset: data[0].offset, color: _.first(data.color)});
                data.splice(0, 0, {offset: data[0].offset, color: last.color});
            }
        }


        if (_.get(this.options, 'masks.max') || this.options.discrete) {
            let last = _.last(data);
            if (last.color === 'NODATA' || !this.options.discrete) {
                data.push({offset: _.last(data).offset, color: '#353C58'});
                data.push({offset: _.last(data).offset, color: '#353C58'});
            } else {
                data.push({offset: _.last(data).offset, color: last.color});
            }
        }

        return data;
    }
}

export default (app) => {
    class GraphService {
        constructor($q) {
            'ngInject';
            this.graphs = new Map();
            this._graphPromises = new Map();
            this.$q = $q;
        }

        register(el, id, options) {
            if (el.nodeName !== 'svg') {
                throw new Error('graphService requires an svg element.');
            }
            let graph = new SinglebandHistogram(el, id, options, this.$q);
            this.graphs = this.graphs.set(id, graph);
            if (this._graphPromises.has(id)) {
                this._graphPromises.get(id).forEach((promise) => {
                    promise.resolve(graph);
                });
                this._graphPromises.delete(id);
            }
            return graph;
        }

        deregister(id) {
            this.getGraph(id).then((graph) => {
                if (this._graphPromises.has(id)) {
                    this._graphPromises.get(id).forEach((promise) => {
                        promise.reject('Graph has been deleted');
                    });
                }
                graph.callbacks.forEach((value, key) => {
                    graph.off(key);
                });
            });
            this.graphcs = this.graphs.delete(id);
        }

        getGraph(id) {
            return this.$q((resolve, reject) => {
                if (this.graphs.has(id)) {
                    resolve(this.graphs.get(id));
                } else if (this._graphPromises.has(id)) {
                    const promises = this._graphPromises.get(id);
                    promises.push({resolve, reject});
                    this._graphPromises = this._graphPromises.set(id, promises);
                } else {
                    this._graphPromises = this._graphPromises.set(id, [{resolve, reject}]);
                }
            });
        }
    }
    app.service('graphService', GraphService);
};
