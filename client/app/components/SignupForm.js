import { Field, Control, Label, Input, Icon, Help, RadioGroup, Button } from 'react-bulma-components/full';


const SignUpForm = () => (
    <div>
        <Field>
            <Label>
                Name
</Label>
            <Control>
                <Input placeholder="Text input" />
            </Control>
        </Field>
        <Field>
            <Label>
                Username
</Label>
            <Control>
                <Input color="success" placeholder="Text input" value="bulma" />
            </Control>
            <Help color="success">
                This username is available
</Help>
        </Field>
        <Field>
            <Label>
                Email
</Label>
            <Control>
                <Input
                    color="danger"
                    type="email"
                    placeholder="Email input"
                    value="hello@"
                />
            </Control>
            <Help color="danger">
                This email is invalid
</Help>
        </Field>
        <Field>
            <Label>
                With Icons
</Label>
            <Control iconLeft iconRight>
                <Input color="success" type="email" placeholder="I have icons" />
                <Icon align="left" icon="bars" />
                <Icon align="right" icon="bars" />
            </Control>
            <Help color="danger">
                This email is invalid
</Help>
        </Field>
        <Field>
            <Label>
                Subject
</Label>
            <Control>
                <Select>
                    <option>
                        Select dropdown
</option>
                    <option>
                        With options
</option>
                </Select>
            </Control>
        </Field>
        <Field>
            <Label>
                Message
</Label>
            <Control>
                <Textarea placeholder="Textarea" />
            </Control>
        </Field>
        <Field>
            <Control>
                <Checkbox>
                    I agree to the
<a href="#agree">
                        terms and conditions
</a>
                </Checkbox>
            </Control>
        </Field>
        <Field>
            <RadioGroup />
        </Field>
        <Field kind="group">
            <Control>
                <Button type="primary">
                    Submit
</Button>
            </Control>
            <Control>
                <Button color="link">
                    Cancel
</Button>
            </Control>
        </Field>
    </div>
)
