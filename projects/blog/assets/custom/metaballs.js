var width = 768;
var height = width * 9 / 16;
var renderer = new THREE.WebGLRenderer();
renderer.setSize(width, height);

document.getElementById("metaballs").appendChild(renderer.domElement);

function MetaBall(xCenter, yCenter, zCenter) {
    this.xCenter = xCenter;
    this.yCenter = yCenter;
    this.zCenter = zCenter;
    this.fun = function(x, y, z) {
        return 1 / Math.sqrt(Math.pow(x - this.xCenter, 2) + Math.pow(y - this.yCenter, 2) + Math.pow(z - this.zCenter, 2));
    };
}
var mb1 = new MetaBall(0, 0, 5);
var mb2 = new MetaBall(0, 0, 0);
var scene = new THREE.Scene();
var camera = new THREE.PerspectiveCamera(45, width / height, 0.1, 10000);
camera.position.x = -10;
camera.position.y = 4;
camera.position.z = -7;
scene.add(camera);
var light = new THREE.PointLight(0xffffff);
light.position.set(-100, 200, 100);
scene.add(light);
var controls = new THREE.OrbitControls(camera, renderer.domElement);
var axes = new THREE.AxisHelper(50);
scene.add(axes);
var geometry = new THREE.Geometry();
var material = new THREE.MeshPhongMaterial({});
var mesh = new THREE.Mesh(geometry, material);
scene.add(mesh);
camera.lookAt(mesh);
var ambLight = new THREE.AmbientLight(0x404040);
scene.add(ambLight);
window.addEventListener('resize', resize);
resize();
animate();

function resize() {
    let w = document.getElementsByTagName("main")[0].offsetWidth;
    let h = w * 9 / 16;
    renderer.setSize(w, h);
    camera.aspect = w / h;
    camera.updateProjectionMatrix();
}
var time = 0;

function animate() {
    time += 0.1;
    mb1.zCenter = (Math.sin(time * 0.4) + 1) * 5;
    mesh.geometry.dispose();
    mesh.geometry = createGeometry(function(x, y, z) {
        return (mb1.fun(x, y, z) + mb2.fun(x, y, z));
    }, 0.5, new THREE.Vector3(-10, -10, -10), new THREE.Vector3(20, 20, 20), 24);
    renderer.render(scene, camera);
    controls.update();
    requestAnimationFrame(animate);
}
