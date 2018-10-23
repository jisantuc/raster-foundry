/* globals console */
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
                height: '120px',
                width: '100%'
            },
            height: 120,
            width: $(this.el).width() || 100,
            margin: {
                top: 5,
                bottom: 5,
                left: 5,
                right: 5
            }
        }, options);

        this.initSvg();
    }


    initSvg() {
        $(this.el).css({
            height: this.options.style.height,
            width: this.options.style.width
        });
    }

    setData(data) {
        this.data = data;
        this.update();
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
            defs = defs.append('defs');
        }

        if (this.data) {
            console.log('rendering with data:', this.data, this.options);

            this.renderAxis(svg, defs);
            this.renderData(svg, defs);
            this.renderGradient(svg, defs);

        } else {
            console.log('Render called with no data');
        }

        super.render();
    }

    renderAxis(svg, defs) {
        console.log('rendering axis', this.options);
        this.xScale = d3.scaleLinear()
            .domain([d3.min(this.data, d => d.x), d3.max(this.data, d => d.x)])
            .range([this.options.margin.left,
                    this.options.width - this.options.margin.right]);
        svg.append('g')
            .call(g => g.attr(
                'transform',
                `translate(0, ${this.options.height - this.options.margin.bottom})`
            ))
            .call(d3.axisBottom(this.xScale));

        this.yScale = d3.scaleLinear()
            .domain([0, d3.max(this.data, d => d.x)])
            .nice()
            .range([this.options.height - this.options.margin.bottom,
                    this.options.margin.top]);

        svg.append('g')
            .call(g => g.attr(
                'transform',
                `translate(${this.options.margin.left}, 0)`
            ))
            .call(d3.axisLeft(this.yScale));
    }

    renderData(svg, defs) {
        svg.append('path')
            .datum(this.data.values)
            .attr('fill', 'none')
            .attr('stroke', 'steelblue')
            .attr('stroke-width', '1.5')
            // .attr('stroke-linejoin', 'round')
            // .attr('stroke-linecap', 'round')
            .attr('d', d3.line().x(v => this.xScale(v.x)).y(v => this.yScale(v.y)));
    }

    renderGradient(svg, defs) {
        let linearGradient = defs.selectAll('linearGradient');
        if (!linearGradient[0] || !linearGradient[0].length) {
            linearGradient = defs.append('linearGradient');
        }

        linearGradient.attr('id', `line-gradient-${this.nodeId}`)
            .attr('gradientUnits', 'userSpaceOnUse')
            .attr('x1', '0%').attr('y1', 0)
            .attr('x2', '100%').attr('y2', 0)
            .selectAll('stop')
            .data(this.data)
            .enter().append('stop')
            .attr('offset', (d) => d.offset)
            .attr('stop-color', (d) => d.color)
            .attr('stop-opacity', (d) => Number.isFinite(d.opacity) ? d.opacity : 1.0);
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
            const graphPromise = this.$q((resolve, reject) => {
                if (this.graphs.has(id)) {
                    resolve(this.graphs.get(id));
                } else if (this._graphPromises.has(id)) {
                    const promises = this._graphPromises.get(id);
                    promises.push(graphPromise);
                    this._graphPromises = this._graphPromises.set(id, promises);
                } else {
                    this._graphPromises = this._graphPromises.set(id, [graphPromise]);
                }
            });
            return graphPromise;
        }
    }
    app.service('graphService', GraphService);
};
