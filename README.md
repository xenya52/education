## Notes

"CICD is not IaC and IaC is not SLSA"

### IaC basics

IaC:

- Manage Infrastrucutre with code
- Defines a template that can be reused
- Resources like servers, networks and databases defined using code
- Code can be version controlled tested an deployed

Imperative Approach

- Define the operations required to achieve a certain infrastructure state

```
Api api = Api.CreateInstance()
api.setProperty(true)
```

Declarative Approach

- Define the state of the infrastructure what it supposed to be

```
def instance {
    property: true
}
```

1993 - CFE Engine
2007 - Cloud Computing (AWS)
2010 - Azure VM launch
2013 - Docker release
2014 - Kubernetes release
2017 - Pulumi tool release
2023 - Opentofu (Teraform but proper OpenSource licence) era

DevOps (Like the CICD livesycle)

Dev -

1. Planning
2. Coding
3. Building
4. Testing

\- Ops

5. Release
6. Deploy
7. Operate
8. Monitor

Important principels

- Continuous -Integration, -Deployment, -Testing
- IaC overall
- Supervision

## Proceed CICD pipeline

### 0 Planning steps / notes

1. code = Rust √
2. build = cargo √
3. testing = (Rust integr.) !
4. release = github actions
5. deploy = docker
6. operate = azure container images / ansible / Terraform / Kubernetes
7. monitor = azure app insights / graphana (don't work without prometeus)

"it's important to make it wrong, for the moment"

### 1 First CICD Pipeline "cicd-hello-world"

#### 1. Pull a rust project from github

First step was to use on the [Project](https://github.com/marcker/rust-api-hello-world).

This worked without any problems and I removed the .github file from the pulled reposetory directory.

#### 2. Create,adjust a dockerfile / setup jenkins

##### 2.1 Adjust Dockerfile

At the beginning I searched for a rust image that I can use to build my project
This worked without any problems.

The next step was to use a server to run my restapi as webapp.

I tried to use debian:bullseye but this is not what I wanted.

##### 2.2 Setup jenkins

Start the raspi

Update system and install jenkins

#### 3. Check for the Start
#### 4. Run Minikube
#### 5. Connect docker application with my local minikube
