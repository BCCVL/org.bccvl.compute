pipeline {

    agent {
        docker {
            image 'hub.bccvl.org.au/bccvl/bccvlbase:2017-02-20'
        }
    }

    stages {

        stage('Build') {

            steps {
                // environment {} is executed in node context, and there is no WORKSPACE defined
                withPyPi() {
                    sh 'rm -fr .eggs .cache'
                    sh 'pip install -e .'
                }
            }

        }
        stage('Test') {

            steps {
                withPyPi() {
                    // install test dependencies
                    sh 'pip install .[test]'
                    // install test runnor
                    sh 'pip install pytest pytest-cov'
                    // TODO: use --cov-report=xml -> coverage.xml
                    sh(script: 'pytest -v --junitxml=junit.xml --cov-report=html --cov=org.bccvl.compute',
                       returnStatus: true)
                }
                // capture test result
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
                publishHTML(target: [
                    allowMissing: false,
                    alwaysLinkToLastBuild: false,
                    keepAll: true,
                    reportDir: 'htmlcov',
                    reportFiles: 'index.html',
                    reportName: 'Coverage Report'
                ])

            }

        }

        stage('Package') {
            when {
                expression {
                    // check if we want to publish a package
                    return publishPackage(currentBuild.result, env.BRANCH_NAME)
                }
            }
            steps {
                sh 'rm -rf build; rm -rf dist'
                withPyPi() {
                    // Build has to happen in correct folder or setup.py won't find MANIFEST.in file and other files
                    sh 'python setup.py register -r devpi sdist bdist_wheel upload -r devpi'
                }
            }
        }

    }

    post {
        always {
            echo "This runs always"

            // does this plugin get committer emails by themselves?
            // alternative would be to put get commiter email ourselves, and list of people who need to be notified
            // and put mail(...) step into each appropriate section
            // => would this then send 2 emails? e.g. changed + state email?
            step([
                $class: 'Mailer',
                notifyEveryUnstableBuild: true,
                recipients: 'gerhard.weis@gmail.com ' + emailextrecipients([
                    [$class: 'CulpritsRecipientProvider'],
                    [$class: 'RequesterRecipientProvider']
                ])
            ])
        }
        success {
            echo 'This will run only if successful'
            //triggerDownstream('org.bccvl.movelib', env.BRANCH_NAME)
        }
        failure {
            echo 'This will run only if failed'
        }
        unstable {
            echo 'This will run only if the run was marked as unstable'
        }
        changed {
            echo 'This will run only if the state of the Pipeline has changed'
            echo 'For example, the Pipeline was previously failing but is now successful'
        }
    }

}
