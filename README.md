# Komponenty

This is a repository for the college project.

## Installation
### Requirements
The project needs:
- running Docker engine
- access to kubernetes cluster (may be local as well, for testing purposes)
- installed kubectl 
### Setting up the environment
For the whole application to run on a Kubernetes cluster, user must create his own image providing a proper config file containing credentials to the Kubernetes cluster.

1. Download the repository and go to the App folder:
```bash
$ git clone https://github.com/maxiwoj/komponenty
$ cd komponenty/App
```
2. There is a Makefile, that can help to set up an environment (on Mac). On other systems, the user must manually set up the environment. This step copies the config file from the default location on Mac. On OS X (when using docker-for-mac kubernetes), just type:
```bash
$ make setup
```
3. A message will be displayed, to adjust the config file (user must change the server of kubernetes cluster in the config file to the displayed one (`docker.for.mac.localhost:6443`)
4. Next step is to build and push the docker image to the docker hub (by default the image is `maxwojczuk/komponenty-rest`), but user may change this by setting a Makefile variable `IMAGE_ADDRESS`. After setting properly the dockerHub image address, you can just type:
```bash
$ make push
```
This will create a docker image and push it to the repository.
5. One more thing to remember is to change the image id in the file `komponenty/backend/deployment.yaml` since it points to the default image (`maxwojczuk/komponenty-rest`).

## Usage
### Running the application
To run the application in a kubernetes cluster (after creating a docker image with credentials for the cluster) just type:
```bash
$ make run-kubernetes
```
This will create an instance of the application, persistent volume and a service with exposed port for interaction from outside of the cluster. To check the port of the host machine, where the service has been exposed, type:
```bash
$ kubectl get services

NAME               TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)          AGE
flask-deployment   NodePort    10.104.183.198   <none>        5000:32307/TCP   6s
kubernetes         ClusterIP   10.96.0.1        <none>        443/TCP          7d
```
In the case above, the host machine port number, where the app is running is 32307. Now you can interact with the application through the HTTP protocol using this port.

### HTTP endpoints



