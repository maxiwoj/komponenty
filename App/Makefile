
IMAGE_ADDRESS=maxwojczuk/komponenty-rest:latest

setup:
	@echo "WARNING: This is for docker-for-desktop kubernetes engine, development only"
	cp ~/.kube/config ./config
	@echo "Remember to properly setup config file"
	@echo "kubernetes engine docker-for-desktop on mac:"
	@echo "change server address to: \'docker.for.mac.localhost:6443\'"

create-image:
	docker build -t komponenty-rest:latest .

push: create-image
	docker tag komponenty-rest:latest $(IMAGE_ADDRESS)
	docker push $(IMAGE_ADDRESS)

run: 
	docker run -p 5000:5000 komponenty-rest:latest

run-kubernetes:
	kubectl create -f ../backend/persistent_vol.yaml
	kubectl create -f ../backend/porsistent_vol_claim.yaml
	kubectl create -f ../backend/deployment.yaml
	kubectl expose deployment/flask-deployment --type="NodePort" --port 5000

clean-kubernetes:
	kubectl delete -f ../backend/persistent_vol.yaml
	kubectl delete -f ../backend/porsistent_vol_claim.yaml
	kubectl delete -f ../backend/deployment.yaml
	kubectl delete service/flask-deployment

delete-jobs:
	kubectl delete jobs --all


