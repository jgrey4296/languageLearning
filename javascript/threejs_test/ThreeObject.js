define(['three'],function(three){

  var ThreeObject = function(colour,position){
    this.geometry = new THREE.BoxGeometry(1,1,1);
    this.material = new THREE.MeshBasicMaterial({ color: colour});
    this.mesh = new THREE.Mesh(this.geometry,this.material);
    if(position !== undefined){
      this.mesh.position.x = position[0];
      this.mesh.position.y = position[1];
      this.mesh.position.z = position[0];
    }
    
  };

  ThreeObject.prototype.addToScene = function(scene){
    scene.add(this.mesh);
  };

  ThreeObject.prototype.rotate = function(){
    this.mesh.rotation.x += 0.1;
    this.mesh.rotation.y += 0.1;
  };

  return ThreeObject;
});