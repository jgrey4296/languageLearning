requirejs.config({
  paths : {
    three : 'libs/three.min',
  },
  shim:{
    three: {
      exports : 'THREE'
    }
  }
});


require(['three','ThreeObject'],function(THREE,Tho){
  //Basic Setup:
  var scene = new THREE.Scene();
  var camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );
  var renderer = new THREE.WebGLRenderer();
  renderer.setSize( window.innerWidth, window.innerHeight );
  document.body.appendChild( renderer.domElement );

  var firstCube = new Tho(0xff00ff);
  var secondCube = new Tho(0xffff00,[1,1,0]);

  firstCube.addToScene(scene);
  secondCube.addToScene(scene);

camera.position.z = 5;


function render() {
  requestAnimationFrame( render );
  firstCube.rotate();
  secondCube.rotate();
  renderer.render( scene, camera );
}
render();
});