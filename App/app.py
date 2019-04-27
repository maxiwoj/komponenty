from flask import Flask, request, abort, jsonify
from wtforms import Form, DecimalField

app = Flask(__name__)


@app.route("/")
def hello():
    return "Hello World!"


@app.route("/run_script", methods=['POST'])
def runScript():
    # TODO: Request a script run through Kubernetes API
    form = InputParameters(request.form)
    if form.validate():
        return "OK"
    else:
        return abort(400)

@app.route("/get_results")
def getResults():
    # TODO: Get the results and return them to the user
    return "Not ready yet"


class InputParameters(Form):
    # TODO: Which parametes are required, what are the constraints?
    # oral parameters:
    # oral bolus dose [mg]
    oral_dose = DecimalField('oral_dose', default=880)
    inf_dose = DecimalField('inf_dose', default=0)
    inf_time = DecimalField('inf_time', default=2)

    # define number of individuals:
    individual_count = DecimalField('individual_count', default=1)
    # define number of females:
    female_count = DecimalField('female_count', default=0)

    # define age range [years]:
    min_age = DecimalField('min_age', default=24)
    max_age = DecimalField('max_age', default=24)

    # #time of the end of simulation [h]:
    t_end = DecimalField('t_end', default=15)

    seed = DecimalField('seed', default=1111)
