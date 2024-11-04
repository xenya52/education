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

### 0 Planning steps

1. code = Rust
2. build = cargo
3. testing = (Rust integr.)
4. release = github actions
5. deploy = docker
6. operate = azure container images
7. monitor = azure app insights
