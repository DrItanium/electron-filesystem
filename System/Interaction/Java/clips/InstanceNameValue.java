package clips;

public class InstanceNameValue extends InstanceValue
{
	/**********************/
	/* InstanceNameValue: */
	/**********************/
	public InstanceNameValue()
	{
		super(new String(""));
	}

	/**********************/
	/* InstanceNameValue: */
	/**********************/
	public InstanceNameValue(
			String value)
	{
		super(value);
	}

	/****************/
	/* lexemeValue: */
	/****************/
	public String lexemeValue() throws Exception
	{
		return (String) getValue();
	}

	/**********************/
	/* instanceNameValue: */
	/**********************/
	public String instanceNameValue() throws Exception
	{
		return (String) getValue();
	}

	/***********/
	/* retain: */
	/***********/
	public void retain()
	{
		//System.out.println("InstanceNameValue retain");
	}

	/*************/
	/* release: */
	/*************/
	public void release()
	{
		//System.out.println("InstanceNameValue release");
	}

	/*************/
	/* toString: */
	/*************/
	public String toString()
	{        
		return "[" + super.toString() + "]";
	}
}
