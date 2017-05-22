node('docker') {

    try {

        stage('Checkout') {
            // clean git clone, but don't fail in case it doesn't exist yet
            sh(script: 'git clean -x -d -f', returnStatus: true)
            checkout scm
        }

        // start up build container
        def img = docker.image('hub.bccvl.org.au/bccvl/bccvlbase:2017-05-18')
        docker.withRegistry('https://hub.bccvl.org.au', 'hub.bccvl.org.au') {
            img.inside() {

                withVirtualenv() {

                    stage('Build') {
                        sh '. ${VIRTUALENV}/bin/activate; pip install -e .'
                    }

                    stage('Test') {
                        // install test dependencies
                        sh '. ${VIRTUALENV}/bin/activate; pip install -e .[test]'
                        // install test runner
                        sh '. ${VIRTUALENV}/bin/activate; pip install pytest pytest-cov'
                        // run tests
                        sh(script: '. ${VIRTUALENV}/bin/activate; pytest -v -junit=junit.xml --cov-report=xml --cov=org.bccvl.compute',
                           returnStatus: true)

                        // capture result
                        step([
                            $class: 'XUnitBuilder',
                            thresholds: [
                                [$class: 'FailedThreshold', failureThreshold: '0',
                                                            unstableThreshold: '1']
                            ],
                            tools: [
                                [$class: 'JUnitType', deleteOutputFiles: true,
                                                      failIfNotNew: true,
                                                      pattern: 'junit.xml',
                                                      stopProcessingIfError: true]
                            ]
                        ])
                        // publish html coverage report
                        step([$class: 'CoberturaPublisher',
                            coberturaReportFile: 'coverage.xml']
                        )
                    }

                    stage('Package') {
                        if (publishPackage(currentBuild.result, env.BRANCH_NAME)) {

                            sh 'rm -fr build dist'
                            sh '. ${VIRTUALENV}/bin/activate; python setup.py register -r devpi sdist bdist_wheel --universal upload -r devpi'

                        }
                    }
                }
            }
        }
    } catch(err) {
        throw err
    } finally {
        sh 'git clean -x -d -f'

        step([
            $class: 'Mailer',
            notifyEveryUnstableBuild: true,
            recipients: 'gerhard.weis@gmail.com ' + emailextrecipients([
                [$class: 'CulpritsRecipientProvider'],
                [$class: 'RequesterRecipientProvider']
            ])
        ])
    }
}
