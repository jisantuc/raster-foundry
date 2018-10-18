import * as d3 from 'd3';
import angular from 'angular';
import d3ContainerTpl from './d3Container.html';

const D3ContainerComponent = {
    templateUrl: d3ContainerTpl,
    controller: 'D3ContainerController',
    bindings: {
    }
};

class D3ContainerController {
    constructor($scope, $log, $element, uuid4) {
        $scope.autoInject(this, arguments);
    }

    $onInit() {
        this.$log('d3Container initialized');
        this.id = this.uuid4.generate();
    }
}

const D3ContainerModule = angular.module('components.histogram.d3Container', []);
D3ContainerModule.component('rfD3Container', D3ContainerComponent);
D3ContainerModule.controller('D3ContainerController', D3ContainerController);

export default D3ContainerModule;
