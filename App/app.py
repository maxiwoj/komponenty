from __future__ import print_function

import random
import string

from flask import Flask, request, abort, jsonify
from kubernetes import client, config
from wtforms import Form, DecimalField
import time
import kubernetes.client
from kubernetes.client.rest import ApiException
from pprint import pprint

# Configure API key authorization: BearerToken
config.load_kube_config()
configuration = kubernetes.client.Configuration()
# configuration.api_key['authorization'] = 'YOUR_API_KEY'
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['authorization'] = 'Bearer'

# create an instance of the API class
api_instance = kubernetes.client.BatchV1Api(
    kubernetes.client.ApiClient(configuration))
app = Flask(__name__)


@app.route("/")
def main():
    return "Hello World!"


@app.route("/jobs", methods=['POST'])
def runScript():
    job_params = []
    # for inputParameters in request.form['jobsParams']:
    #     form = InputParameters(inputParameters)
    #     if form.validate():
    #         job_params.append(form)
    #     else:
    #         return abort(400)
    # TODO: Request a script run through Kubernetes API
    container_image = "gregwator/komponenty"
    name = id_generator()
    body = kube_create_job_object(name, container_image, env_vars={"VAR": "TESTING"})
    try:
        api_response = api_instance.create_namespaced_job("default", body, pretty=True)
        print(api_response)
    except ApiException as e:
        print("Exception when calling BatchV1Api->create_namespaced_job: %s\n" % e)
    return "OK"


@app.route("/job/<jobId>", methods=['DELETE'])
def deleteJob(jobId):
    name = 'name_example'  # str | name of the Job
    namespace = 'namespace_example'  # str | object name and auth scope, such as for teams and projects
    pretty = 'pretty_example'  # str | If 'true', then the output is pretty printed. (optional)
    body = kubernetes.client.V1DeleteOptions()  # V1DeleteOptions |  (optional)
    dry_run = 'dry_run_example'  # str | When present, indicates that modifications should not be persisted. An invalid or unrecognized dryRun directive will result in an error response and no further processing of the request. Valid values are: - All: all dry run stages will be processed (optional)
    grace_period_seconds = 56  # int | The duration in seconds before the object should be deleted. Value must be non-negative integer. The value zero indicates delete immediately. If this value is nil, the default grace period for the specified type will be used. Defaults to a per object value if not specified. zero means delete immediately. (optional)
    orphan_dependents = True  # bool | Deprecated: please use the PropagationPolicy, this field will be deprecated in 1.7. Should the dependent objects be orphaned. If true/false, the \"orphan\" finalizer will be added to/removed from the object's finalizers list. Either this field or PropagationPolicy may be set, but not both. (optional)
    propagation_policy = 'propagation_policy_example'  # str | Whether and how garbage collection will be performed. Either this field or OrphanDependents may be set, but not both. The default policy is decided by the existing finalizer set in the metadata.finalizers and the resource-specific default policy. Acceptable values are: 'Orphan' - orphan the dependents; 'Background' - allow the garbage collector to delete the dependents in the background; 'Foreground' - a cascading policy that deletes all dependents in the foreground. (optional)

    try:
        api_response = api_instance.delete_namespaced_job(name, namespace,
                                                          pretty=pretty,
                                                          body=body,
                                                          dry_run=dry_run,
                                                          grace_period_seconds=grace_period_seconds,
                                                          orphan_dependents=orphan_dependents,
                                                          propagation_policy=propagation_policy)
        return api_response
    except ApiException as e:
        return abort(500,
                     "Exception when calling BatchV1Api->delete_namespaced_job: %s\n" % e)


@app.route("/jobs", methods=["GET"])
def getJobs():
    namespace = 'namespace_example'  # str | object name and auth scope, such as for teams and projects
    pretty = 'pretty_example'  # str | If 'true', then the output is pretty printed. (optional)
    _continue = '_continue_example'  # str | The continue option should be set when retrieving more results from the server. Since this value is server defined, kubernetes.clients may only use the continue value from a previous query result with identical query parameters (except for the value of continue) and the server may reject a continue value it does not recognize. If the specified continue value is no longer valid whether due to expiration (generally five to fifteen minutes) or a configuration change on the server, the server will respond with a 410 ResourceExpired error together with a continue token. If the kubernetes.client needs a consistent list, it must restart their list without the continue field. Otherwise, the kubernetes.client may send another list request with the token received with the 410 error, the server will respond with a list starting from the next key, but from the latest snapshot, which is inconsistent from the previous list results - objects that are created, modified, or deleted after the first list request will be included in the response, as long as their keys are after the \"next key\".  This field is not supported when watch is true. Clients may start a watch from the last resourceVersion value returned by the server and not miss any modifications. (optional)
    field_selector = 'field_selector_example'  # str | A selector to restrict the list of returned objects by their fields. Defaults to everything. (optional)
    label_selector = 'label_selector_example'  # str | A selector to restrict the list of returned objects by their labels. Defaults to everything. (optional)
    limit = 56  # int | limit is a maximum number of responses to return for a list call. If more items exist, the server will set the `continue` field on the list metadata to a value that can be used with the same initial query to retrieve the next set of results. Setting a limit may return fewer than the requested amount of items (up to zero items) in the event all requested objects are filtered out and kubernetes.clients should only use the presence of the continue field to determine whether more results are available. Servers may choose not to support the limit argument and will return all of the available results. If limit is specified and the continue field is empty, kubernetes.clients may assume that no more results are available. This field is not supported if watch is true.  The server guarantees that the objects returned when using continue will be identical to issuing a single list call without a limit - that is, no objects created, modified, or deleted after the first request is issued will be included in any subsequent continued requests. This is sometimes referred to as a consistent snapshot, and ensures that a kubernetes.client that is using limit to receive smaller chunks of a very large result can ensure they see all possible objects. If objects are updated during a chunked list the version of the object that was present at the time the first list result was calculated is returned. (optional)
    resource_version = 'resource_version_example'  # str | When specified with a watch call, shows changes that occur after that particular version of a resource. Defaults to changes from the beginning of history. When specified for list: - if unset, then the result is returned from remote storage based on quorum-read flag; - if it's 0, then we simply return what we currently have in cache, no guarantee; - if set to non zero, then the result is at least as fresh as given rv. (optional)
    timeout_seconds = 56  # int | Timeout for the list/watch call. This limits the duration of the call, regardless of any activity or inactivity. (optional)
    watch = True  # bool | Watch for changes to the described resources and return them as a stream of add, update, and remove notifications. Specify resourceVersion. (optional)
    try:
        api_response = api_instance.list_namespaced_job(namespace,
                                                        pretty=pretty,
                                                        _continue=_continue,
                                                        field_selector=field_selector,
                                                        label_selector=label_selector,
                                                        limit=limit,
                                                        resource_version=resource_version,
                                                        timeout_seconds=timeout_seconds,
                                                        watch=watch)
        return api_response
    except ApiException as e:
        return abort(500, (
            "Exception when calling BatchV1Api->list_namespaced_job: %s\n" % e))


@app.route("/jobs/status", methods=["GET"])
def getJobsStatus():
    return "Not ready yet"


@app.route("/jobs/results", methods=["GET"])
def getJobsResults():
    # TODO: Get the results and return them to the user
    return "OK"


def kube_create_job_object(name, container_image, namespace="default", container_name="jobcontainer", env_vars={}):
    """
    Create a k8 Job Object
    Minimum definition of a job object:
    {'api_version': None, - Str
    'kind': None,     - Str
    'metadata': None, - Metada Object
    'spec': None,     -V1JobSpec
    'status': None}   - V1Job Status
    Docs: https://github.com/kubernetes-client/python/blob/master/kubernetes/docs/V1Job.md
    Docs2: https://kubernetes.io/docs/concepts/workloads/controllers/jobs-run-to-completion/#writing-a-job-spec

    Also docs are pretty pretty bad. Best way is to ´pip install kubernetes´ and go via the autogenerated code
    And figure out the chain of objects that you need to hold a final valid object So for a job object you need:
    V1Job -> V1ObjectMeta
          -> V1JobStatus
          -> V1JobSpec -> V1PodTemplate -> V1PodTemplateSpec -> V1Container

    Now the tricky part, is that V1Job.spec needs a .template, but not a PodTemplateSpec, as such
    you need to build a PodTemplate, add a template field (template.template) and make sure
    template.template.spec is now the PodSpec.
    Then, the V1Job.spec needs to be a JobSpec which has a template the template.template field of the PodTemplate.
    Failure to do so will trigger an API error.

    Also Containers must be a list!

    Docs3: https://github.com/kubernetes-client/python/issues/589
    """
    # Body is the object Body
    body = client.V1Job(api_version="batch/v1", kind="Job")
    # Body needs Metadata
    # Attention: Each JOB must have a different name!
    body.metadata = client.V1ObjectMeta(namespace=namespace, name=name)
    # And a Status
    body.status = client.V1JobStatus()
    # Now we start with the Template...
    template = client.V1PodTemplate()
    template.template = client.V1PodTemplateSpec()
    # Passing Arguments in Env:
    env_list = []
    for env_name, env_value in env_vars.items():
        env_list.append(client.V1EnvVar(name=env_name, value=env_value))
    container = client.V1Container(name=container_name, image=container_image, env=env_list)
    template.template.spec = client.V1PodSpec(containers=[container], restart_policy='Never')
    # And finaly we can create our V1JobSpec!
    body.spec = client.V1JobSpec(ttl_seconds_after_finished=600, template=template.template)
    return body


def kube_create_job():
    # Create the job definition
    container_image = "namespace/k8-test-app:83226641581a1f0971055f972465cb903755fc9a"
    name = id_generator()
    body = kube_create_job_object(name, container_image, env_vars={"VAR": "TESTING"})
    try:
        api_response = api_instance.create_namespaced_job("default", body, pretty=True)
        print(api_response)
    except ApiException as e:
        print("Exception when calling BatchV1Api->create_namespaced_job: %s\n" % e)
    return


def id_generator(size=12, chars=string.ascii_lowercase + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))


if __name__ == "__main__":
    app.run(host='127.0.0.1', port=5000)

class InputParameters(Form):
    # TODO: Which parametes are required, what are the constraints?
    # oral parameters:
    # oral bolus dose [mg]
    oral_dose = DecimalField('oral_dose', default=880)
    inf_dose = DecimalField('inf_dose', default=0)
    inf_time = DecimalField('inf_time', default=2)
    #
    # # define number of individuals:
    # individual_count = DecimalField('individual_count', default=1)
    # # define number of females:
    # female_count = DecimalField('female_count', default=0)
    #
    # # define age range [years]:
    # min_age = DecimalField('min_age', default=24)
    # max_age = DecimalField('max_age', default=24)

    # #time of the end of simulation [h]:
    t_end = DecimalField('t_end', default=15)

    seed = DecimalField('seed', default=1111)
